--v2 Notes
--Review output from Legal Organization. Column used is o.FriendlyName which may result in not matching data from customer's Atlas OrgName.
--Remove any NULL values found in Custom Fields. Still working to output those as blank instead of NULL.


USE [SnowLicenseManager]
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


BEGIN
			   declare @CID INT=1
               declare @UserID INT=1
               declare @Parameters NVARCHAR(MAX) = NULL
               declare @FromWeb bit = 0
               declare @q nvarchar(MAX)
               declare @qp nvarchar(MAX)
               set @qp = N'@CID int, @UserID int, @lang nvarchar(10),@canViewUsernames bit, @UserCurrencyRate numeric(18,4), @UserCurrency nvarchar(10)';
               set nocount on;
               set dateformat ymd
               
               IF @Parameters IS NOT NULL
                              SET @Parameters = REPLACE(@Parameters,'@CID',CONVERT(nvarchar,@CID))
               ELSE
                              SET @Parameters = ''

               DECLARE             @canViewUsernames BIT
               SET                        @canViewUsernames = (SELECT CASE WHEN (LOWER(IsNull(dbo.GetSystemSetting(@CID, 'ANONYMOUS_USER_MANAGEMENT'),'false')) = 'true' OR dbo.CheckObjectAccess(@CID, @UserID, 'VIEWUSERNAMES') = 0)
                                                                                                                                                                     THEN 0
                                                                                                                                                                     ELSE 1
                                                                                                                                                      END)

                              
               -- Get user currency
               declare @UserCurrency nvarchar(10)
               set @UserCurrency = dbo.GetUserCurrency(@CID, @UserID)

               declare @UserCurrencyRate numeric(18,4)
               set @UserCurrencyRate = dbo.GetUserCurrencyrate(@CID,@UserID)
                                                                                                                                       
               declare @cfColumns nvarchar(max);
               set @cfColumns = dbo.GetCustomFieldTableColumns(@cid, 3, 'cf', 1, @UserID, 1);
               
               declare @lang nvarchar(10);
               set @lang = dbo.GetUserLanguage(@cid,@userid) ;
               
               set @q = N'

               with maintenanacePeriods as (
            select
            mp.CID,
            mp.LicenseID,
            isnull(mp.PeriodFrom, cp.ValidFrom) as PeriodFrom,
            isnull(mp.PeriodTo, cp.ValidTo) as PeriodTo,
            mp.notes,
            mp.InvoiceReference,
            mp.UpgradeRights,
            case when getdate() between isnull(mp.PeriodFrom, cp.ValidFrom) and isnull(mp.PeriodTo, cp.ValidTo) then 1
                            else 0 end as isactive,
            row_number() over (partition by mp.LicenseID order by isnull(mp.PeriodTo, cp.ValidTo) desc) as RowID
            from 
                            dbo.tblMaintenancePeriod mp
                            inner join dbo.tblLicense l on 
                                        l.CID = mp.CID and 
                                        l.LicenseID = mp.LicenseID
                            left outer join dbo.tblContractPeriod cp on 
                                        cp.CID = mp.CID and 
                                        cp.PeriodID = mp.ContractPeriodID
            where 
                            getdate() between isnull(mp.PeriodFrom, cp.ValidFrom) and isnull(mp.PeriodTo, cp.ValidTo)
                            or
                            isnull(mp.PeriodTo, cp.ValidTo) <= getdate()
                                                        )
                              select
                                             mp.CID,
                                             mp.LicenseID,
                                             mp.PeriodFrom,
                                             mp.PeriodTo,
                                             mp.UpgradeRights,
                                             mp.Notes,
                                             mp.InvoiceReference,
                                             mp.isactive
                              into #maintenancePeriodsActive
                              from
                                                                                          maintenanacePeriods mp
                              where 
                                                                                          mp.RowID = 1

               
               ; with invalidAssignments as (
                              select distinct
                                             CID,
                                             LicenseID,
                                             Quantity as InvalidAssignments,
                                             (CASE WHEN (ilpp.Reasons & 4) > 0 THEN 1 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 8) > 0 THEN 2 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 16) > 0 THEN 4 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 32) > 0 THEN 8 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 64) > 0 THEN 16 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 128) > 0 THEN 32 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 256) > 0 THEN 64 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 512) > 0 THEN 128 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 2048) > 0 THEN 256 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 4096) > 0 THEN 512 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 8192) > 0 THEN 1024 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 16384) > 0 THEN 2048 ELSE 0 END) |
                                             (CASE WHEN (ilpp.Reasons & 32768) > 0 THEN 32768 ELSE 0 END)
                                                            as InvalidAssignmentReasons,
                                             (CASE WHEN (ilpp.Reasons & 64508) != 0 THEN 1 ELSE 0 END)
                                                            as HasInvalidAssignments
                                             from tblInvalidLicensesPerPurchase ilpp
                                             where
                                                            (ilpp.Reasons & 64508) != 0 -- 64508 is the bitwise AND operation for all InvalidAssignmentReasons flags
               ),
               legalLicenseCount as
               (
                              select
                                             l.CID,
                                             l.LicenseID,
                                             case sign(l.LegalLicenseCount)
                                                            when -1 then 0
                                             else 
                                                            l.LegalLicenseCount
                                             end as LegalLicenseCount,                                         
                                             cpl.LicensePurchaseCost
                              from
                                             [dbo].[tblLicense] l
                                             inner join [dbo].[tblSystemUserOrgDefinition] def on
                                                            def.CID = l.CID
                                                            and def.UserID = @UserID
                                                            and def.OrgChecksum = l.LegalOrgChecksum
                                             left outer join [dbo].[tblCostsPerLicenseSummary] cpl on
                                                            cpl.CID = l.CID
                                                            and cpl.LicenseID = l.LicenseID                                                                
               ),
               distributedLicenseCount as
               (
                              select                   
                                             lod.CID,
                                             lod.LicenseID,
                                             sum(lod.LicenseCount) as LicenseCount,
                                             sum(cplo.LicensePurchaseCost) as UserPurchaseValue
                              from
                                             [dbo].[tblLicenseOrgDistribution] lod
                                             inner join [dbo].[tblSystemUserOrgDefinition] def on
                                                            def.CID = lod.CID
                                                            and def.UserID = @UserID
                                                            and def.OrgChecksum = lod.OrgChecksum
                                             left outer join [dbo].[tblCostsPerLicenseAndOrganization] cplo on
                                                            cplo.CID = lod.CID
                                                            and cplo.LicenseID = lod.LicenseID
                                                            and cplo.OrgChecksum = lod.OrgChecksum
                              group by
                                             lod.CID,
                                             lod.LicenseID                                                                                 
               ),
			   upgradedLicenseTracking as
			   (
							select
                                             upt.CID,
											 upt.LicenseID,
                                             upt.UpgradedLicenseID,
											 upt.UpgradedLegalLicenseCount
                              from
                                             dbo.tblLicenseUpgrades upt
			   ),
               upgradedLicenses as
               (
                              select
                                             lu.CID,
											 lu.LicenseID,
                                             lu.UpgradedLicenseID,
                                             sum(LegalLicenseCount) as UpgradedLicenseCount
                              from
                                             dbo.tblLicenseUpgrades lu
                              group by
                                             lu.CID,
											 lu.LicenseID,
                                             lu.UpgradedLicenseID
               ),
               unassignedLicenses as 
               (
                              select
                                             lic.CID,
                                             lic.LicenseID,
                                             lic.AssignmentType as AssignmentType,
                                             sum(ult.Quantity) as TotalUnassignedLicenses
                              from tblLicense lic
                              inner join tblUnassignedLicenseTracking ult
                                             on  ult.CID = lic.CID
                                             and ult.LicenseID = lic.LicenseID
                              where
                                             lic.CID = @CID                   
                                             and lic.LegalLicenseCount > 0
                              group by
                                             lic.CID, lic.LicenseID, lic.AssignmentType
               ),
               maintenanceAndSupportCost as 
               (
                              select
                                             l.CID,
                                             l.LicenseID,
                                             sum(CONVERT(numeric(18,2),(cpl.AccumulatedSupportCost*@UserCurrencyRate))) as TotalSupportCost,
                                       sum(CONVERT(numeric(18,2),(cpl.AccumulatedMaintenanceCost*@UserCurrencyRate))) as TotalMaintenanceCost
                              from 
                                             dbo.tblLicense l 
                              left outer join dbo.tblCostsPerLicenseSummary cpl on
                                             cpl.CID = l.CID
                                             and cpl.LicenseID = l.LicenseID
                              group by 
                                             l.CID, l.LicenseID
               ),
               namedusers as(
                              select l.cid, l.licenseid, stuff((select '',['' + u.UserName + '']'' from tbllicenseapplicationgrants lag
                                             inner join tbluser u on lag.cid = u.cid and lag.userid = u.userid
                                             where lag.cid = l.cid and lag.licenseid = l.licenseid
                              for xml path ('''')),1,1, '''') as NamedUser,
                              lagu.users as NumberOfUsers
                              from tbllicense l
                              inner join (select cid,licenseid,count(userid) as users from tbllicenseapplicationgrants
                                                            group by cid, licenseid) lagu on l.cid = lagu.cid and l.licenseid = lagu.licenseid
               ),
               hasdocs as (
                              select cid,
                              parentid,
                              count(documentid) as NumberOfDocs from tbldocuments 
                              where documenttype = 1
                              group by cid, parentid
               ),
               lurs as ( -- query for bit valued Userights
                              select cid,licenseid,[4] as HasdowngradeRights,[8] as HasCrossedRights,[32] as hasVMrights, [262144] as IsCrossPlatform from (
                                             select distinct l.cid, l.licenseid, lurs.useright 
                                             from
                                             dbo.tblLicense l
                                             inner join dbo.tblLicenseUseRightSettings lurs on 
                                                            lurs.CID = l.CID
                                                            and lurs.LicenseID = l.LicenseID
                                                            and ((lurs.UseRight in (4,8,262144)
                                                            and lurs.bitvalue = 1)
                                                            or (lurs.UseRight = 32))
                              ) p 
                              PIVOT (count(useright) for useright in ([4],[8],[32],[262144])) as pvt
               )/*,
               crossPlatform as
               (
                              select distinct
                                             l.CID,
                                             l.LicenseID,
                                             convert(bit, (CASE WHEN lurs.UseRight is not null and lurs.UseRight = 262144 THEN 1 ELSE 0 END) ) as IsCrossPlatform
                              from
                                             dbo.tblLicense l
                                             inner join dbo.tblLicenseUseRightSettings lurs on 
                                                            lurs.CID = l.CID
                                                            and lurs.LicenseID = l.LicenseID
                                                            and lurs.UseRight = 262144
               ),
               downgrade as
               (
                              select distinct
                                             l.CID,
                                             l.LicenseID,
                                             convert(bit, (CASE WHEN lurs.UseRight is not null and lurs.UseRight = 4 THEN 1 ELSE 0 END) ) as HasDowngradeRights
                              from
                                             dbo.tblLicense l
                                             inner join dbo.tblLicenseUseRightSettings lurs on 
                                                            lurs.CID = l.CID
                                                            and lurs.LicenseID = l.LicenseID
                                                            and lurs.UseRight = 4
               )*/';
               set @q = @q + N'
               
               select distinct
                              l.LicenseID AS ''License ID'',
                              -- DEFAULT COLUMNS 
                              a.Name AS ''Application Name'',
							  o.FriendlyName AS ''Legal Organization'',
							  --''ROOT'' as ''Legal Organisation'', --USE to force ROOT for all Licenses
							  ISNULL(CONVERT(VARCHAR(10), l.PurchaseDate, 120), '''') AS ''Purchase Date'',
							  
							  --Left for troubleshooting
							  --l.[AssignmentType] AS ''Assignment Type'',

							  CASE
								WHEN l.[AssignmentType] = 0 THEN ''Organization''
								WHEN l.[AssignmentType] = 1 THEN ''Computer/datacenter''
								WHEN l.[AssignmentType] = 2 THEN ''User''
								WHEN l.[AssignmentType] = 3 THEN ''Site''
							  END AS ''Assignment Type'',

							  --Left for troubleshooting
							  --CAST(l.Metric as varchar) AS ''Metric'',

							  CASE
								WHEN l.Metric = 0 THEN ''Custom Compare Value''
								WHEN l.Metric = 1 THEN ''Installations''
								--WHEN l.Metric = 2 THEN ''2''
								--WHEN l.Metric = 3 THEN ''3''
								--WHEN l.Metric = 4 THEN ''4''
								--WHEN l.Metric = 5 THEN ''5''
								--WHEN l.Metric = 6 THEN ''6''
								WHEN l.Metric = 7 THEN ''Number of processors''
								WHEN l.Metric = 8 THEN ''Number of processor cores''
								WHEN l.Metric = 9 THEN ''Users''
								WHEN l.Metric = 10 THEN ''Devices''
								WHEN l.Metric = 11 THEN ''Concurrent users''
								WHEN l.Metric = 12 THEN ''Concurrent devices''
								WHEN l.Metric = 13 THEN ''PVU''
								WHEN l.Metric = 14 THEN ''CAL (Client Access License)''
								ELSE ''Review License''
							  END AS ''Metric'',

							  --isnULL(l.IsUpgrade,0) as ''Is Upgrade'',
							  CASE 
							  WHEN l.IsUpgrade = 0 THEN ''NO''
							  WHEN l.IsUpgrade = 1 THEN ''YES''
							  ELSE ''NO''
							  END AS ''Is Upgrade'',

							  --isnull(upt.LicenseID,0) as ''Upgrade From License ID'',
							  CASE 
							    WHEN upt.LicenseID IS NOT NULL THEN CONVERT(VARCHAR(20), upt.UpgradedLicenseID) 
								ELSE ''''
								END AS ''Upgrade From License ID'',
								

							  --isnull(upt.UpgradedLegalLicenseCount, 0) as ''Base License Quantity to Upgrade'',
							  ISNULL(CONVERT(VARCHAR(20), upt.UpgradedLegalLicenseCount), '''') AS ''Base License Quantity to Upgrade'',

							  --l.LegalLicenseCount AS ''Quantity'',
							  --ISNULL(CONVERT(VARCHAR(20), l.LegalLicenseCount), '''') AS ''Quantity'',
							  CASE 
						      WHEN lu.UpgradedLicenseCount <> '''' THEN CONVERT(VARCHAR(20), lu.UpgradedLicenseCount + l.LegalLicenseCount)
							  ELSE CONVERT(VARCHAR(20), l.LegalLicenseCount)
							  END AS ''Quantity'',

							  ISNULL(CONVERT(VARCHAR(10), l.SubscriptionValidFrom, 120), '''') AS ''Subscription Valid From'',
							  ISNULL(CONVERT(VARCHAR(10), l.SubscriptionValidTo, 120), '''') AS ''Subscription Valid To'',
							  isnull(l.IsSubscription, 0) as ''Is Subscription'',

							  ISNULL(CAST(ROUND(((CASE
									WHEN (licCount.LegalLicenseCount IS NOT NULL) THEN licCount.LicensePurchaseCost
									ELSE ISNULL(distributedLicCount.UserPurchaseValue, l.PurchaseValueBase)
							  END) * uc.BaseCurrencyRate), 2) AS VARCHAR(20)), '''') as ''Purchase Price'',
							  l.PurchaseCurrency as ''Purchase Currency'',
							  --''Manually Currency'' as ''Purchase Currency'',
							  convert(bit, isnull(lurs.IsCrossPlatform, 0)) as ''Cross Platform Rights'',
							  convert(bit, isnull(lurs.HasCrossedRights, 0)) as ''Cross Edition Rights'',
							  convert(bit, isnull(lurs.HasDowngradeRights, 0)) as ''Downgrade Rights'',
							  isnull(l.IsAutoAllocated,0) as ''Auto allocate (distribute) license'',
							  isnull(l.AutoAllocateOnce,0) as ''Auto allocate license only once'',
							  isnull(mp.UpgradeRights, 0) as ''Maintenance Includes Upgrade Rights'',
							  CASE 
								WHEN l.MaintenanceAccordingtoAgreement IS NULL THEN 1
								ELSE 0
                              END AS ''Maintenance According to Agreement'',
							  ISNULL(CONVERT(VARCHAR(10), mp.PeriodFrom, 120), '''') AS ''Maintenance and Support Valid From'',
							  ISNULL(CONVERT(VARCHAR(10), mp.PeriodTo, 120), '''') AS ''Maintenance and Support Valid To'',
							  isnULL(c.[AssignedID],'''') as ''Agreement Number'',
							  isnULL(l.[ExternalID],'''') as ''External ID'',
							  isnULL(l.[Vendor],'''') as ''Vendor/Reseller'',
							  isnULL(l.[LicenseProofLocation],'''') as ''License Proof Location'',
							  isnULL(l.Notes,'''') as ''Purchase Notes'',
							  isnULL(l.[LicenseKeys],'''') as ''Serial numbers/license keys'',
							  isnULL(l.[MediaStorage],'''') as ''Installation Media Location'',
							  isnULL(l.[InvoiceReference],'''') as ''Purchase Invoice Reference'',
							  ISNULL(l.ProductDescription, skur.ProductName) as ''Product Description'',
							  isnULL(msc.TotalMaintenanceCost,0) as ''Maintenance Cost'',
							  isnULL(msc.TotalSupportCost,0) as ''Support Cost'',
							  isnULL(mp.Notes,'''') as ''Maintenance and Support Notes'',
							  isnULL(mp.InvoiceReference,'''') as ''Maintenance and Support Invoice''
                              ' + @cfColumns;
               set @q = @q + N'
               FROM
                              [dbo].[tblLicense] l
                              left outer join lurs on
                                             lurs.cid = l.cid 
                                             and lurs.licenseid = l.licenseid
                              OUTER APPLY (SELECT MAX(uc1.basecurrencyrate) basecurrencyrate from tblCurrency uc1 where
                                                            l.cid = uc1.cid
                                                            and @usercurrency = uc1.CurrencyName
                                                            and l.PurchaseDate >= uc1.ValidFrom
                                                            and l.PurchaseDate <= isnull(uc1.ValidTo,''21000101'')) uc
                              left outer join hasdocs on
                                             hasdocs.cid = l.cid
                                             and hasdocs.parentid = convert(nvarchar(36),l.licenseid)
                              /*left outer join downgrade on
                                             downgrade.CID = l.CID
                                             and downgrade.LicenseID = l.LicenseID
                              left outer join crossPlatform crossPlat on
                                             crossPlat.CID = l.CID
                                             and crossPlat.LicenseID = l.LicenseID*/
                              left outer join     [dbo].[tblApplication] a on
                                             a.ApplicationID = l.ApplicationID
                              left outer join     [dbo].[tblManufacturer] m on
                                             m.ManufacturerID = a.ManufacturerID
                              inner join [dbo].[tblOrganization] o on
                                             o.CID = l.CID
                                             and o.OrgChecksum = l.LegalOrgChecksum
                              inner join [dbo].[tblSystemUserOrgDefinition] def on
                                             def.CID = l.CID
                                             and def.UserID = @UserID
                                             and def.OrgChecksum = l.LegalOrgChecksum
                              left outer join     [dbo].[tblContract] c on
                                             c.CID = l.CID 
                                             AND c.ContractID = l.ContractID
                              left outer join dbo.tblApplicationInfo ai on
                                             ai.CID = l.CID 
                                             AND ai.ApplicationID = l.ApplicationID
                              left outer join dbo.tblApplicationSecondaryMetric asm on
                                             asm.CID = l.CID 
                                             AND asm.ApplicationID = l.ApplicationID
                                             AND asm.PreferredMetric = l.Metric
                              left outer join dis.tblapplication_version dav
                                             on l.applicationid = dav.applicationid
                              left outer join     tblContractPeriod cp on 
                                             cp.CID = @CID
                                             and cp.ContractID = l.ContractID
                                             and
                                             (
                                                            dateadd(day, datediff(day, 0, getdate()), 0) 
                                                                           between dateadd(day, datediff(day, 0, cp.ValidFrom), 0) 
                                                                           AND dateadd(day, datediff(day, 0, cp.ValidTo), 0)
                                             )
                              left outer join     tblContractPeriod cp2 on 
                                             cp2.CID = @CID
                                             and cp2.ContractID = l.ContractID
                                             and 
                                             (
                                                            dateadd(day, datediff(day, 0, l.PurchaseDate), 0) 
                                                                between dateadd(day, datediff(day, 0, cp2.ValidFrom), 0) 
                                                                           and dateadd(day, datediff(day, 0, cp2.ValidTo), 0)
                                             )
                              
                              --            Get LegalLicenseCount if user has right to LegalOrgId
                              left outer join legalLicenseCount licCount on
                                             licCount.CID = l.CID
                                             and licCount.LicenseID = l.LicenseID
                                                                                                                                                      
                              --            Get sum of distributed license count for OrgIds the user has rights for
                              left outer join distributedLicenseCount distributedLicCount on
                                             distributedLicCount.CID = l.CID
                                             AND distributedLicCount.LicenseID = l.LicenseID
                              left outer join dbo.tblSKURepository skur ON 
                                             skur.SKUID = l.SKUID
                              left outer join dbo.' + dbo.GenerateCustomFieldTemporaryName(@cid, 3) + N' cf on
                                             cf.ElementID = l.LicenseID
                                             and cf.UserID = @UserID
                              left outer join upgradedLicenses lu on
                                             lu.CID = l.CID
                                             and lu.UpgradedLicenseID = l.LicenseID
							  left outer join upgradedLicenseTracking upt on
											 upt.CID = l.CID
											 and upt.LicenseID = l.LicenseID
                              left outer join dbo.tblCostsPerLicenseSummary cpl on
                                             cpl.CID = l.CID
                                             and cpl.LicenseID = l.LicenseID    
                              left join unassignedLicenses as ul on 
                                             ul.CID = l.CID 
                                             and ul.LicenseID = l.LicenseID
                              left outer join #maintenancePeriodsActive mp on
                                             mp.CID = l.CID and
                                             mp.LicenseID = l.LicenseID
                              left outer join maintenanceAndSupportCost msc on
                                             msc.CID = l.CID and
                                             msc.LicenseID = l.LicenseID
                              left outer join invalidAssignments ia on
                                             ia.CID = l.CID and
                                             ia.LicenseID = l.LicenseID
                              left outer join namedusers nu on 
                                             nu.cid = l.cid and nu.licenseid = l.licenseid
               where
                              l.CID = @CID
                              ' + isnull(@parameters, N'') + N';
                              
               DROP TABLE #maintenancePeriodsActive; ';

               execute sp_executesql @q, @qp, @CID, @UserID, @lang, @canViewUsernames, @UserCurrencyRate, @UserCurrency;
END
