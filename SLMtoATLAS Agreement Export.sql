--NOTES
--v2 build. This will pull the necessary values from an SLM SQL. Note the CID,USERID you use as this is default to 1,1 respectively.
--Review Legal Organization. When Org values are missing, this script will default the value to ROOT.
--Ensure that all NULL values are removed from the output document prior to using in upload.


USE [SnowLicenseManager]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

BEGIN

	declare @CID INT = 1
	declare @UserID INT = 1
	declare @Parameters NVARCHAR(MAX) = NULL
	declare @FromWeb bit = 0

	set nocount on;
	set dateformat ymd

	-- Get user language
	declare @UserLang nvarchar(10)
	set @UserLang = dbo.GetUserLanguage(@CID, @UserID)

	declare @IsAdmin int
	set		@IsAdmin = dbo.CheckObjectAccess(@CID, @UserID, 'FULLACCESS');
	
	-- Get user currency
	declare @UserCurrency nvarchar(10)
	set @UserCurrency = dbo.GetUserCurrency(@CID, @UserID)

	declare @UserCurrencyRate numeric(18,4)
	set @UserCurrencyRate = dbo.GetUserCurrencyrate(@CID,@UserID)
	
	declare @cfColumns nvarchar(max);
	set @cfColumns = dbo.GetCustomFieldTableColumns(@cid, 4, 'cf', 1, @UserID, 1);


	if @Parameters IS NOT NULL
		set @Parameters = replace(@Parameters,'@CID',convert(nvarchar,@CID))
	else
		set @Parameters = ''

	declare @lang nvarchar(10);

	set @lang = (select [Language] from tblSystemUser where UserID = @UserID);

	declare @q nvarchar(max);
	declare @qp nvarchar(max)

	set @qp = N'@CID int, @UserID int, @lang nvarchar(10), @UserCurrencyRate numeric(18,4), @IsAdmin int, @UserCurrency nvarchar(10)'

	

	set @q = N'
	;
	with lc as -- licensecount
	(
		select
			l.CID,
			l.ContractID,
			count(*) as LicensePurchaseCount,
			sum(isnull(cpl.licensequantity,0)) as Licensequantity,
			sum(isnull(cpl.licensepurchasecost,0)*uc.basecurrencyrate ) as Licensepurchasecost,
			sum(isnull(cpl.currentmaintenancecost,0)) as currentmaintenancecost,
			sum(isnull(cpl.currentsupportcost,0)) as currentsupportcost,
			sum(isnull(cpl.accumulatedsupportcost,0)) as totalsupportcost,
			sum(isnull(cpl.accumulatedmaintenancecost,0)) as totalmaintenancecost,
			sum(isnull(l.purchasevaluebase,0)*uc.basecurrencyrate) as licensecost,
			sum(isnull(cpl.TotalCost,0)) as totalcost,
			stuff(( select distinct '',['' + InvoiceReference + '']'' from tbllicense where ContractID = l.ContractID and cid = l.cid and invoicereference is not null and InvoiceReference <> ''''
				for xml path ('''')),1,1, '''') as InvoiceReferences
		from
			dbo.tblLicense l
		OUTER APPLY (SELECT MAX(uc1.basecurrencyrate) basecurrencyrate from tblCurrency uc1 where
				l.cid = uc1.cid
				and @usercurrency = uc1.CurrencyName
				and l.PurchaseDate >= uc1.ValidFrom
				and l.PurchaseDate <= isnull(uc1.ValidTo,''21000101'')) uc
		left outer join dbo.tblCostsPerLicenseSummary cpl on l.cid = cpl.cid and l.licenseid = cpl.licenseid
		group by
			l.CID,
			l.ContractID
	),
	cc as -- computercount
	(
		select
			cc.CID,
			cc.ContractID,
			count(*) as ComputerCount,
			sum(isnull(ci.purchasevaluebase,0)*uc.basecurrencyrate) as PurchaseValueBase
		from 
			dbo.tblContractComputers cc
		left outer join tblcomputerinfo ci on cc.cid = ci.cid and cc.computerid = ci.computerid
		OUTER APPLY (SELECT MAX(uc1.basecurrencyrate) basecurrencyrate from tblCurrency uc1 where
				ci.cid = uc1.cid
				and @usercurrency = uc1.CurrencyName
				and ci.PurchaseDate >= uc1.ValidFrom
				and ci.PurchaseDate <= isnull(uc1.ValidTo,''21000101'')) uc
		group by
			cc.CID,
			cc.ContractID
	),
	oc as -- objectcount
	(
		select
			co.CID,
			co.ContractID,
			count(*) as ObjectCount
		from
			dbo.tblContractObjects co 
		group by
			co.CID,
			co.ContractID
	)'

	set @q = @q + N'

	select
		CASE
        WHEN vr.VendorName IS NULL OR vr.VendorName = '''' THEN
            CASE
                WHEN c.Manufacturer IS NULL OR c.Manufacturer = '''' THEN ''TBD''
                ELSE c.Manufacturer
            END
        ELSE vr.VendorName
    END AS Manufacturer,
		c.[Name] as ''Agreement Name'',
		c.AssignedID As ''Agreement Number'',
		ISNULL((SELECT mc.AssignedID FROM tblContract mc WHERE mc.ContractID = c.MasterContractID ),'''') as ''Master Agreement Number'',
		CASE
			WHEN c.ContractType = 0 THEN ''SOFTWARE''
			WHEN c.ContractType = 1 THEN ''MAINTENANCE''
			WHEN c.ContractType = 2 THEN ''SUPPORT''
			WHEN c.ContractType = 3 THEN ''PURCHASE''
			WHEN c.ContractType = 4 THEN ''CUSTOM''
			WHEN c.ContractType = 5 THEN ''ORACLE''
			ELSE ''Unknown'' -- Update as I use this
			END AS ''Agreement Type'',
		CASE
			WHEN c.IsUpgradable = 0 THEN ''NO''
			WHEN c.IsUpgradable = 1 THEN ''YES''
			ELSE ''NULL''
		END AS ''Activate Automatic Upgrade'',
		CASE
			WHEN c.IsSubscription = 0 THEN ''NO''
			WHEN c.IsSubscription = 1 THEN ''YES''
			ELSE ''NULL''
		END AS ''Subscription Agreement'',
		ISNULL(CONVERT(varchar, cp.ValidFrom, 120), '''') as ''Valid From'', --output is dd/mm/yyyy
		ISNULL(CONVERT(varchar, cp.ValidTo, 120), '''') as ''Valid To'',  --output is dd/mm/yyyy
		c.AlertOnExpiration as ''Alert On Expiration'',
		--''ROOT'' as LegalOrganisation, --USE if Organization is being defaulted to ROOT
		COALESCE(o.FriendlyName, ''ROOT'') as ''Legal Organisation'', --This will default missing values to ROOT
		CASE
			WHEN c.RestrictOrganization = 0 THEN ''NO''
			WHEN c.RestrictOrganization = 1 THEN ''YES''
			ELSE ''NULL''
		END AS ''Restrict Organization''
		' + @cfColumns + ' --Working to Remove NULL values from output
	FROM
		dbo.tblContract c
		LEFT OUTER JOIN dbo.tblContractPeriod cp ON 
			cp.CID = c.CID
			AND cp.ContractID = c.ContractID
			AND 
			(
				DATEADD(day, DATEDIFF(day, 0, GETDATE()), 0) 
					BETWEEN DATEADD(day, DATEDIFF(day, 0, cp.ValidFrom), 0) 
					AND DATEADD(day, DATEDIFF(day, 0, cp.ValidTo), 0)
			)
		LEFT OUTER JOIN dbo.tblContractPeriod cp2 ON 
			cp2.CID = c.CID
			AND cp2.ContractID = c.ContractID
			AND cp2.ValidTo = 
			(
				SELECT TOP 1 
					MAX(cpm.ValidTo) 
				FROM 
					dbo.tblContractPeriod cpm 
				WHERE 
					cpm.CID = c.CID
					AND cpm.ContractID = cp2.ContractID 
			)
		LEFT OUTER JOIN	[dbo].[tblVendorAssignments] va ON 
			va.CID = c.CID 
			AND va.ElementID = c.ContractID 
			AND va.ElementType = 1
		LEFT OUTER JOIN	[dbo].[tblVendorRepository] vr ON 
			vr.CID = c.[CID] 
			AND vr.VendorID = va.VendorID
		LEFT OUTER JOIN	[dbo].[tblOrganization] o ON 
			o.[CID] = c.CID
			AND c.[OrgChecksum] = o.[OrgChecksum]		
		LEFT OUTER JOIN	[dbo].[tblParameterTranslation] pt ON 
			pt.ParameterType = 7 
			AND pt.ParameterValue = c.ContractType 
			AND pt.[Language] = @lang
		LEFT OUTER JOIN	[dbo].[tblContractCustomTypes] cct ON 
			cct.CID = c.CID 
			AND cct.TypeID = c.ContractType
		left outer join dbo.' + dbo.GenerateCustomFieldTemporaryName(@cid, 4) + N' cf on
			cf.ElementID = c.ContractID
			and cf.UserID = @UserID				
		left outer join lc on
			lc.CID = c.CID
			and lc.ContractID = c.ContractID		
		left outer join cc on
			cc.CID = c.CID
			and cc.ContractID = c.ContractID	
		left outer join oc on
			oc.CID = c.CID
			and oc.ContractID = c.ContractID					
	WHERE 
		c.CID = @CID
		AND 
		(
			IsNull(c.RestrictOrganization, 0) = 0
			OR c.ContractID IN 
			(
				SELECT
					c1.[ContractID]
				FROM 
					[dbo].[tblContract] c1
					INNER JOIN [dbo].[tblSystemUserOrgDefinition] def ON 
						def.CID = c.CID 
						AND def.UserID = @UserID
						AND def.OrgChecksum = c.OrgChecksum 
				WHERE 
					c1.CID = c.CID
					AND IsNull(c.RestrictOrganization, 0) > 0
			)
		)
		AND
		(
			ISNULL(c.RestrictToGroups, 0) = 0
			OR @IsAdmin = 1
			OR EXISTS
			(
				SELECT
					1
				FROM
					[dbo].[tblContractGroups] cg
					INNER JOIN[dbo].[tblSystemUserGroups] sug ON
						sug.UserID = @UserID 
						AND cg.GroupID = sug.GroupID
				WHERE
					cg.CID = @CID
					AND cg.[ContractID] = c.[ContractID]
			)
		)
		
		'
		+ isnull(@parameters, N'') + N';'

	exec sp_executesql @q, @qp, @CID, @UserID, @lang, @UserCurrencyRate, @IsAdmin, @UserCurrency;

END
