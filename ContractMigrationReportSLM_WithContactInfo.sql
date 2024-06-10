--NOTES
--v2 build. This will pull the necessary values from an SLM SQL. Note the CID,USERID you use as this is default to 1,1 respectively.
--Review Legal Organization. When Org values are missing, this script will default the value to ROOT.
--Ensure that all NULL values are removed from the output document prior to using in upload.
USE SnowLicenseManager
GO

IF EXISTS ( SELECT * 
            FROM   sysobjects 
            WHERE  id = object_id(N'[dbo].[StockReportContractMigrationReport]') 
                   and OBJECTPROPERTY(id, N'IsProcedure') = 1 )
	BEGIN
		DROP PROCEDURE [dbo].[StockReportContractMigrationReport]
	END

SET ANSI_NULLS, QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[StockReportContractMigrationReport]
(
	@CID		int,
	@UserID		int,
	@FromWeb bit = 0,
	@Parameters	nvarchar(max) = NULL
)
WITH ENCRYPTION AS
BEGIN

	-- Setup.
	set dateformat ymd;
	

	declare	@lang nvarchar(10);
	

	declare @UserCurrency nvarchar(10)
	set @UserCurrency = dbo.GetUserCurrency(@CID, @UserID)
                                                                                                                

						
	DECLARE	@canViewUsernames BIT
	SET		@canViewUsernames = (SELECT	CASE WHEN (LOWER(IsNull(dbo.GetSystemSetting(@CID, 'ANONYMOUS_USER_MANAGEMENT'),'false')) = 'true' OR dbo.CheckObjectAccess(@CID, @UserID, 'VIEWUSERNAMES') = 0)
											THEN 0
											ELSE 1
										END)

	-- Get user currency
	DECLARE @UserCurrencyRate numeric(18,4)
	SET @UserCurrencyRate = IsNull((SELECT TOP 1 c.BaseCurrencyRate
									  FROM tblCurrency c
									 WHERE c.CurrencyName = dbo.GetUserCurrency(@CID,@UserID)
									   AND c.CID = @CID
									 ORDER BY ValidFrom DESC ),0)

	declare @cfColumns nvarchar(max);
	set @cfColumns = dbo.GetCustomFieldTableColumns(@cid, 4, 'cf', 1, @UserID, 1);

	IF IsNull(@UserCurrencyRate,0) = 0
		SET @UserCurrencyRate = 1

	SET @lang = dbo.GetUserLanguage(@CID, @UserID);

	set nocount on;
	set dateformat ymd

	-- Get user language
	declare @UserLang nvarchar(10)
	set @UserLang = dbo.GetUserLanguage(@CID, @UserID)

	declare @IsAdmin int
	set		@IsAdmin = dbo.CheckObjectAccess(@CID, @UserID, 'FULLACCESS');
	



	if @Parameters IS NOT NULL
		set @Parameters = replace(@Parameters,'@CID',convert(nvarchar,@CID))
	else
		set @Parameters = ''

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
	               hasdocs as (
                              select cid,
                              parentid,
                              count(documentid) as NumberOfDocs from tbldocuments 
                              where documenttype = 2
                              group by cid, parentid
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
		c.ContractID As ContractID,
		c.ManufacturerContact as ''Manufacturer_Contact'',
		c.ManufacturerLink as ''Manufacturer_Website'',
		CASE
			WHEN c.IsValidForAll = 0 THEN ''NO''
			WHEN c.IsValidForAll = 1 THEN ''YES''
			ELSE ''NULL''
		END AS ''Searchable_Outside_Period'',
		c.ManufacturerContactPhone as ''Manufacturer_Contact_Phone_Number'',
		c.ManufacturerContactEmail as ''Manufacturer_Contact_Email'',
		c.LocalContact as ''Local_Contact'',
		c.LocalContactDepartment as ''Local_Contact_Department'',
		c.LocalContactPhone as ''Local_Contact_Phone'',
		c.LocalContactEmail as ''Local_Contact_Email'',
		c.RenewalDays as ''Renewal_Days'',
		CASE 
		WHEN ISNULL(c.AlertOnExpiration, 0) = 1 THEN ''YES''
		ELSE ''NO''
		END AS Alert_On_Expiration,
		c.[Description] as ''Contract_Description'',
		c.WarningDayLimit as ''Warning_Day_Limit'',
		c.CriticalDayLimit as ''Critical_Day_Limit'',
		hasdocs.NumberOfDocs As ''NumberOfAttachedDocs'',
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
		ISNULL(CONVERT(varchar, cp.ValidFrom, 103), '''') as ''Valid From'', --output is dd/mm/yyyy
		ISNULL(CONVERT(varchar, cp.ValidTo, 103), '''') as ''Valid To'',  --output is dd/mm/yyyy
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
			left outer join hasdocs on
             hasdocs.cid = c.cid
            and hasdocs.parentid = convert(nvarchar(36),c.contractID)
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
GO
--Insert the report definition in tblReport


DECLARE @ParameterID				uniqueidentifier = (SELECT NEWID())													
DECLARE @ParameterName				NVARCHAR(MAX) = NULL
DECLARE @ParameterType				NVARCHAR(MAX) = NULL
DECLARE @ParameterBaseSql			NVARCHAR(MAX) = NULL
DECLARE @ParameterIsStock			int = 1

DECLARE @ReportID					uniqueidentifier = (SELECT NEWID())
DECLARE @StockReport				int = 0
DECLARE @IsCustomReport				int = 1
DECLARE @ReportType					int = 0
DECLARE @ViewName					NVARCHAR(MAX) = 'PROCEDURE2'
DECLARE @Name						NVARCHAR(MAX) = 'Snow Contract Migration Report'
DECLARE @Description				NVARCHAR(MAX) = 'Data points that can be mapped correctly to the Snow Import Template'
DECLARE @SQLQuery					NVARCHAR(MAX) = '[StockReportContractMigrationReport]'
DECLARE @ColumnList					NVARCHAR(MAX) = 'ContractID,Manufacturer,Agreement Name,Agreement Number,Master Agreement Number,Agreement Type,Activate Automatic Upgrade,Subscription Agreement,Valid From,Valid To,Alert On Expiration,Legal Organisation,Restrict Organization,NumberOfAttachedDocs,Manufacturer_Contact,Manufacturer_Website,Manufacturer_Contact_Phone_Number,Manufacturer_Contact_Email,Local_Contact,Local_Contact_Department,Local_Contact_Phone,Local_Contact_Email,Renewal_Days,Alert_On_Expiration,Contract_Description,Warning_Day_Limit,Critical_Day_Limit,Searchable_Outside_Period'
DECLARE @FunctionList				NVARCHAR(MAX) = NULL
DECLARE @OrderList					NVARCHAR(MAX) = NULL
DECLARE @Criteria					NVARCHAR(MAX) = NULL
DECLARE @GroupList					NVARCHAR(MAX) = NULL
DECLARE @GroupSummaryList			NVARCHAR(MAX) = NULL
DECLARE @ColumnVisibility			NVARCHAR(MAX) = '1,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1'
DECLARE @KeyFieldName				NVARCHAR(MAX) = 'ContractID'
DECLARE @RowTargetLink				NVARCHAR(MAX) = 'Contract.aspx?id='
DECLARE @KeysCacheID				NVARCHAR(MAX) = NULL
DECLARE @UsesCustomFields			int = 1
DECLARE @CustomFieldCategoryID		int = 0
DECLARE @TranslateName				NVARCHAR(MAX) = NULL
DECLARE @TranslateDescription		NVARCHAR(MAX) = NULL
DECLARE @MultiEditPage				NVARCHAR(MAX) = NULL
DECLARE @DocumentType				int = 0
DECLARE @DocumentParentFieldName	NVARCHAR(MAX) = NULL
DECLARE @StatisticsReport			int = 0

Delete from tblReportParametersLink
WHERE ReportID in 
(Select ReportID from tblReport
Where Name = 'Snow Contract Migration Report')

Delete from tblReport
WHERE ReportID in 
(Select ReportID from tblReport
Where Name = 'Snow Contract Migration Report')

IF EXISTS(SELECT * FROM tblReport Where Name = @NAME)
BEGIN
	SET @ReportID = (SELECT TOP 1 ReportID FROM tblReport Where Name = @NAME)
	UPDATE tblReport SET 
		ReportID=@ReportID,
		StockReport=@StockReport,
		IsCustomReport=@IsCustomReport,
		ReportType=@ReportType,
		ViewName=@ViewName,
		Name=@Name,
		[Description]=@Description,
		SQLQuery=@SQLQuery,
		ColumnList=@ColumnList,
		FunctionList=@FunctionList,
		OrderList=@OrderList,
		Criteria=@Criteria,
		GroupList=@GroupList,
		GroupSummaryList=@GroupSummaryList,
		ColumnVisibility=@ColumnVisibility,
		KeyFieldName=@KeyFieldName,
		RowTargetLink=@RowTargetLink,
		KeysCacheID=@KeysCacheID,
		UsesCustomFields=@UsesCustomFields,
		CustomFieldCategoryID=@CustomFieldCategoryID,
		TranslateName=@TranslateName,
		TranslateDescription=@TranslateDescription,
		MultiEditPage=@MultiEditPage,
		DocumentType=@DocumentType,
		DocumentParentFieldName= @DocumentParentFieldName,
		StatisticsReport = @StatisticsReport
	WHERE ReportID = @ReportID

END
ELSE
BEGIN
	SET @ReportID = NewID()
	INSERT INTO tblReport ([ReportID], [StockReport], [IsCustomReport], [ReportType], [ViewName], [Name], [Description], [SQLQuery], [ColumnList], [FunctionList], [OrderList], [Criteria], [GroupList], [GroupSummaryList], [ColumnVisibility], [KeyFieldName], [RowTargetLink], [KeysCacheID], [UsesCustomFields], [CustomFieldCategoryID], [TranslateName], [TranslateDescription], [MultiEditPage], [DocumentType], [DocumentParentFieldName], [StatisticsReport])
	VALUES (@ReportID, @StockReport, @IsCustomReport, @ReportType, @ViewName, @Name, @Description, @SQLQuery, @ColumnList, @FunctionList, @OrderList, @Criteria, @GroupList, @GroupSummaryList, @ColumnVisibility, @KeyFieldName, @RowTargetLink, @KeysCacheID, @UsesCustomFields, @CustomFieldCategoryID, @TranslateName, @TranslateDescription, @MultiEditPage, @DocumentType, @DocumentParentFieldName, @StatisticsReport)

	INSERT INTO tblReportParametersLink ([ReportID], [ReportParameterID]) VALUES (@ReportID, '4b68714d-0132-442d-b9b9-a61f5f6cb7a2')
	INSERT INTO tblReportParametersLink ([ReportID], [ReportParameterID]) VALUES (@ReportID, '2075a45d-4af2-4b09-89b6-b83debfef187')
	INSERT INTO tblReportParametersLink ([ReportID], [ReportParameterID]) VALUES (@ReportID, 'be496c9c-f4d5-4f26-a2fb-673bc2313c39')
	INSERT INTO tblReportParametersLink ([ReportID], [ReportParameterID]) VALUES (@ReportID, '463056a3-4cfc-42d3-89ba-d9558cc0fa30')
	INSERT INTO tblReportParametersLink ([ReportID], [ReportParameterID]) VALUES (@ReportID, '8c2c3950-1fc5-43fd-8ac3-0610e2b3612c')
END

