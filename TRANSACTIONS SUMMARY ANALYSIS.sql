--PROJECT: POSTPAID AND PREPAID TRANSACTION ANALYSIST AND SUMMARY
--AUTHOR: UBONG ETIEKET
--ANALYZING POSTPAID AND PREPAID COLLECTION PER DISTRICT
---- TRANSACTION SCRIPT FOR TRANS DETAIL PPM/PPD
;WITH PrepayRanked AS (
    SELECT
        pt.*,
        ROW_NUMBER() OVER (
            PARTITION BY pt.SPN
            ORDER BY pt.TokenDate DESC  -- TokenDate desc; TokenID removed (not present)
        ) AS rn
    FROM [DWINCMS].[Fact].[PrepaymentTrans] pt
),
LatestPrepay AS (
    -- pick only the most recent prepayment row per SPN
    SELECT
        SPN,
        TokenDate,
        Tokens,
        Consumption_MWh,
        CashPointCode
    FROM PrepayRanked
    WHERE rn = 1
)

SELECT 
    a.PrePaymentReference,
    a.SPN,
    a.SPN_SECNIS_MOD,
    a.CustomerID,
    a.ContractSequence,
    a.PaymentDate,
    a.PaymentDateKey,
    c.Agency,
    e.CashPoint            AS CHANNELNAME,
    a.Receipt,
    g.ContractService,
    h.PaymentMethod,
    i.PaymentType,
    a.PaymentReference,
    a.AccountingDate,
    a.DebitCredit,
    b.OldAccount           AS CustomerOldAccount,
    b.Surname,
    b.FirstName,
    b.SecondName,
    a.AmountPaid,
    m.TariffId,
    m.Tariff,
    m.MeterNumber,
    m.Feeder,
    m.Transformer,
    m.Band,
    m.TariffCode,
    n.District,
    n.Zone,
    lp.TokenDate          AS LatestTokenDate,
    lp.Tokens             AS LatestTokens,
    lp.Consumption_MWh    AS LatestConsumption_MWh,
    lp.CashPointCode      AS LatestTokenCashPointCode

FROM [DWINCMS].[Fact].[PaymentTrans]        AS a
INNER JOIN [DWINCMS].[Dim].[Customers]     AS b ON a.CustomerID = b.CustomerID
INNER JOIN [DWINCMS].[Dim].[Agency]        AS c ON a.AgencyCode = c.AgencyCode
INNER JOIN [DWINCMS].[Dim].[PaymentStatus] AS d ON a.CurrentPaymentStatusSK = d.PaymentStatusSK
INNER JOIN [DWINCMS].[Dim].[CashPoint]     AS e ON a.CashPointCode = e.CashPointCode
INNER JOIN [DWINCMS].[Dim].[BillType]      AS f ON a.BillTypeSK = f.BillTypeSK
INNER JOIN [DWINCMS].[Dim].[ContractService] AS g ON a.ContractServiceSK = g.ContractServiceSK
INNER JOIN [DWINCMS].[Dim].[PaymentMethod] AS h ON a.PaymentMethodSK = h.PaymentMethodSK
INNER JOIN [DWINCMS].[Dim].[PaymentType]   AS i ON a.PaymentTypeSK = i.PaymentTypeSK

LEFT JOIN [DWINCMS].[Dim].[Contracts]      AS m ON a.SPN = m.SPN
LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy]   AS n ON m.CommOfficeCode = n.CommOfficeCode

-- join to the latest prepayment per SPN (from the CTE)
LEFT JOIN LatestPrepay lp
    ON a.SPN = lp.SPN

WHERE a.AmountPaid <> 0
  AND a.PaymentDate BETWEEN '2025-11-23 00:00:00.000' AND '2025-11-23 23:59:59.999'
  AND h.PaymentMethodSK IN (35, 36)
ORDER BY a.PaymentDate DESC;

 ---- TRANSACTION SCRIPT FOR ONLY PREPAID 
;WITH PrepayRanked AS (
    SELECT pt.*,
           ROW_NUMBER() OVER (PARTITION BY pt.SPN ORDER BY pt.TokenDate DESC) AS rn
    FROM [DWINCMS].[Fact].[PrepaymentTrans] pt
),
LatestPrepay AS (
    SELECT SPN, TokenDate, Tokens, Consumption_MWh, CashPointCode
    FROM PrepayRanked WHERE rn = 1
)
SELECT 
    a.PrePaymentReference, a.SPN, a.SPN_SECNIS_MOD, a.CustomerID, a.ContractSequence,
    a.PaymentDate, a.PaymentDateKey, c.Agency, e.CashPoint AS CashPointName, a.Receipt,
    g.ContractService, h.PaymentMethod, i.PaymentType, a.PaymentReference, a.AccountingDate,
    a.DebitCredit, b.OldAccount AS CustomerOldAccount, b.Surname, b.FirstName, b.SecondName,
    a.AmountPaid, m.TariffId, m.Tariff, m.MeterNumber, m.Feeder, m.Transformer, m.Band, m.TariffCode,
    n.District, n.Zone,
    lp.TokenDate AS LatestTokenDate, lp.Tokens AS LatestTokens, lp.Consumption_MWh AS LatestConsumption_KWh,
    lp.CashPointCode AS LatestTokenCashPointCode
FROM [DWINCMS].[Fact].[PaymentTrans] AS a
INNER JOIN [DWINCMS].[Dim].[Customers]     AS b ON a.CustomerID = b.CustomerID
INNER JOIN [DWINCMS].[Dim].[Agency]        AS c ON a.AgencyCode = c.AgencyCode
INNER JOIN [DWINCMS].[Dim].[PaymentStatus] AS d ON a.CurrentPaymentStatusSK = d.PaymentStatusSK
INNER JOIN [DWINCMS].[Dim].[CashPoint]     AS e ON a.CashPointCode = e.CashPointCode
INNER JOIN [DWINCMS].[Dim].[BillType]      AS f ON a.BillTypeSK = f.BillTypeSK
INNER JOIN [DWINCMS].[Dim].[ContractService] AS g ON a.ContractServiceSK = g.ContractServiceSK
INNER JOIN [DWINCMS].[Dim].[PaymentMethod] AS h ON a.PaymentMethodSK = h.PaymentMethodSK
INNER JOIN [DWINCMS].[Dim].[PaymentType]   AS i ON a.PaymentTypeSK = i.PaymentTypeSK
LEFT JOIN [DWINCMS].[Dim].[Contracts]      AS m ON a.SPN = m.SPN
LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy]   AS n ON m.CommOfficeCode = n.CommOfficeCode
LEFT JOIN LatestPrepay lp ON a.SPN = lp.SPN
WHERE a.AmountPaid <> 0
  AND a.PaymentDate BETWEEN '2025-11-22 00:00:00.000' AND '2025-11-22 23:59:59.999'
  AND h.PaymentMethodSK IN (35,36)
  AND g.ContractService = 'Prepaid'
ORDER BY a.PaymentDate DESC;

 ---- TRANSACTION SCRIPT FOR ONLY POSTPAID

 ;WITH PrepayRanked AS (
    SELECT pt.*,
           ROW_NUMBER() OVER (PARTITION BY pt.SPN ORDER BY pt.TokenDate DESC) AS rn
    FROM [DWINCMS].[Fact].[PrepaymentTrans] pt
),
LatestPrepay AS (
    SELECT SPN, TokenDate, Tokens, Consumption_MWh, CashPointCode
    FROM PrepayRanked WHERE rn = 1
)
SELECT 
    a.PrePaymentReference, a.SPN, a.SPN_SECNIS_MOD, a.CustomerID, a.ContractSequence,
    a.PaymentDate, a.PaymentDateKey, c.Agency, e.CashPoint AS CashPointName, a.Receipt,
    g.ContractService, h.PaymentMethod, i.PaymentType, a.PaymentReference, a.AccountingDate,
    a.DebitCredit, b.OldAccount AS CustomerOldAccount, b.Surname, b.FirstName, b.SecondName,
    a.AmountPaid, m.TariffId, m.Tariff, m.MeterNumber, m.Feeder, m.Transformer, m.Band, m.TariffCode,
    n.District, n.Zone,
    lp.TokenDate AS LatestTokenDate, lp.Tokens AS LatestTokens, lp.Consumption_MWh AS LatestConsumption_KWh,
    lp.CashPointCode AS LatestTokenCashPointCode
FROM [DWINCMS].[Fact].[PaymentTrans] AS a
INNER JOIN [DWINCMS].[Dim].[Customers]     AS b ON a.CustomerID = b.CustomerID
INNER JOIN [DWINCMS].[Dim].[Agency]        AS c ON a.AgencyCode = c.AgencyCode
INNER JOIN [DWINCMS].[Dim].[PaymentStatus] AS d ON a.CurrentPaymentStatusSK = d.PaymentStatusSK
INNER JOIN [DWINCMS].[Dim].[CashPoint]     AS e ON a.CashPointCode = e.CashPointCode
INNER JOIN [DWINCMS].[Dim].[BillType]      AS f ON a.BillTypeSK = f.BillTypeSK
INNER JOIN [DWINCMS].[Dim].[ContractService] AS g ON a.ContractServiceSK = g.ContractServiceSK
INNER JOIN [DWINCMS].[Dim].[PaymentMethod] AS h ON a.PaymentMethodSK = h.PaymentMethodSK
INNER JOIN [DWINCMS].[Dim].[PaymentType]   AS i ON a.PaymentTypeSK = i.PaymentTypeSK
LEFT JOIN [DWINCMS].[Dim].[Contracts]      AS m ON a.SPN = m.SPN
LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy]   AS n ON m.CommOfficeCode = n.CommOfficeCode
LEFT JOIN LatestPrepay lp ON a.SPN = lp.SPN
WHERE a.AmountPaid <> 0
  AND a.PaymentDate BETWEEN '2025-11-22 00:00:00.000' AND '2025-11-22 23:59:59.999'
  AND h.PaymentMethodSK IN (35,36)
  AND g.ContractService IN ('Postpaid MD','Postpaid Non MD')
ORDER BY a.PaymentDate DESC;





----DAILY SUMMARY BY CHANNEL NAME(POSTPAID)
-- Corrected: Dynamic pivot with totals row for Postpaid MD + Postpaid Non MD
DECLARE @cols              NVARCHAR(MAX),
        @displayCols       NVARCHAR(MAX),
        @sumExpr           NVARCHAR(MAX),
        @totalColsDisplay  NVARCHAR(MAX),
        @sql               NVARCHAR(MAX);

-- 1) Build the distinct cashpoint list (quoted) for the PIVOT IN([...])
SELECT @cols = STRING_AGG(QUOTENAME(CashPointName), ',')
FROM (
    SELECT DISTINCT e.CashPoint AS CashPointName
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g ON a.ContractServiceSK = g.ContractServiceSK
    INNER JOIN [DWINCMS].[Dim].[CashPoint] e ON a.CashPointCode = e.CashPointCode
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN '2025-11-29 00:00:00.000' AND '2025-11-29 23:59:59.999'
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService IN ('Postpaid MD','Postpaid Non MD')
) t;

IF @cols IS NULL
BEGIN
    PRINT 'No cashpoints found for the filter; nothing to pivot.';
    RETURN;
END

-- 2) Build display columns and sum expressions and total-row columns from the distinct cashpoints
;WITH DistinctCashPoints AS (
    SELECT DISTINCT e.CashPoint AS CashPointName
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g ON a.ContractServiceSK = g.ContractServiceSK
    INNER JOIN [DWINCMS].[Dim].[CashPoint] e ON a.CashPointCode = e.CashPointCode
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN '2025-11-29 00:00:00.000' AND '2025-11-29 23:59:59.999'
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService IN ('Postpaid MD','Postpaid Non MD')
)
SELECT 
    @displayCols = STRING_AGG(
        'CASE WHEN ' + QUOTENAME(CashPointName) + ' IS NULL THEN '''' ELSE FORMAT(' + QUOTENAME(CashPointName) + ', ''N0'') END AS ' + QUOTENAME(CashPointName),
        ', '
    ),
    @sumExpr = STRING_AGG('ISNULL(' + QUOTENAME(CashPointName) + ', 0)', ' + '),
    @totalColsDisplay = STRING_AGG(
        'FORMAT(SUM(CASE WHEN CashPointName = ''' + REPLACE(CashPointName, '''', '''''') + ''' THEN AmountPaid ELSE 0 END), ''N0'') AS ' + QUOTENAME(CashPointName),
        ', '
    )
FROM DistinctCashPoints;

-- 3) Build dynamic SQL that pivots then formats columns and computes total, and produce a final derived table to ORDER BY
SET @sql = N'
;WITH src AS (
    SELECT 
        n.District,
        e.CashPoint AS CashPointName,
        a.AmountPaid
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g      ON a.ContractServiceSK = g.ContractServiceSK
    INNER JOIN [DWINCMS].[Dim].[CashPoint] e           ON a.CashPointCode = e.CashPointCode
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m            ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n         ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h       ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN ''2025-11-29 00:00:00.000'' AND ''2025-11-29 23:59:59.999''
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService IN (''Postpaid MD'',''Postpaid Non MD'')
)
-- pivot per district
, pivoted AS (
    SELECT p.District, ' + @displayCols + ',
           FORMAT(' + @sumExpr + ', ''N0'') AS TotalAmount
    FROM (
        SELECT District, CashPointName, AmountPaid FROM src
    ) s
    PIVOT (
        SUM(AmountPaid) FOR CashPointName IN (' + @cols + ')
    ) p
)
-- totals row computed directly from src (no pivot needed)
, totals AS (
    SELECT 
        ''TOTAL'' AS District,
        ' + @totalColsDisplay + ',
        FORMAT(SUM(AmountPaid), ''N0'') AS TotalAmount
    FROM src
)
-- final derived result (unioned rows) so we can ORDER by on outer select
, final AS (
    SELECT * FROM pivoted
    UNION ALL
    SELECT * FROM totals
)
SELECT *
FROM final
ORDER BY 
    CASE WHEN District = ''TOTAL'' THEN 1 ELSE 0 END,  -- put TOTAL at bottom
    District;
';

-- 4) Execute the dynamic query
EXEC sp_executesql @sql;



---DAILY COMMULATIVE SUMMARY FOR ALL DISTRICTS BY CHANNEL NAME(prepaid)

-- Dynamic pivot with totals row: Prepaid cumulative AmountPaid by District with CashPoint columns + TOTAL row
DECLARE @cols          NVARCHAR(MAX),
        @displayCols   NVARCHAR(MAX),
        @sumExpr       NVARCHAR(MAX),
        @totalColsDisplay NVARCHAR(MAX),
        @sql            NVARCHAR(MAX);

-- 1) Build the distinct cashpoint list (quoted) for the PIVOT IN([...])
SELECT @cols = STRING_AGG(QUOTENAME(CashPointName), ',')
FROM (
    SELECT DISTINCT e.CashPoint AS CashPointName
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g ON a.ContractServiceSK = g.ContractServiceSK
    INNER JOIN [DWINCMS].[Dim].[CashPoint] e ON a.CashPointCode = e.CashPointCode
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN '2025-11-29 00:00:00.000' AND '2025-11-29 23:59:59.999'
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService = 'Prepaid'
) t;

IF @cols IS NULL
BEGIN
    PRINT 'No cashpoints found for the filter; nothing to pivot.';
    RETURN;
END

-- 2) Build display columns and sum expressions and total-row columns from the distinct cashpoints
;WITH DistinctCashPoints AS (
    SELECT DISTINCT e.CashPoint AS CashPointName
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g ON a.ContractServiceSK = g.ContractServiceSK
    INNER JOIN [DWINCMS].[Dim].[CashPoint] e ON a.CashPointCode = e.CashPointCode
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN '2025-11-29 00:00:00.000' AND '2025-11-29 23:59:59.999'
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService = 'Prepaid'
)
SELECT 
    @displayCols = STRING_AGG(
        -- outer column: show empty string if NULL else formatted number with commas
        'CASE WHEN ' + QUOTENAME(CashPointName) + ' IS NULL THEN '''' ELSE FORMAT(' + QUOTENAME(CashPointName) + ', ''N0'') END AS ' + QUOTENAME(CashPointName),
        ', '
    ),
    @sumExpr = STRING_AGG('ISNULL(' + QUOTENAME(CashPointName) + ', 0)', ' + '),
    @totalColsDisplay = STRING_AGG(
        -- total row: formatted sum for each cashpoint using SUM(CASE ...)
        'FORMAT(SUM(CASE WHEN CashPointName = ''' + REPLACE(CashPointName, '''', '''''') + ''' THEN AmountPaid ELSE 0 END), ''N0'') AS ' + QUOTENAME(CashPointName),
        ', '
    )
FROM DistinctCashPoints;

-- 3) Build dynamic SQL that pivots then formats columns and computes total, and produce a final derived table to ORDER BY
SET @sql = N'
;WITH src AS (
    SELECT 
        n.District,
        e.CashPoint AS CashPointName,
        a.AmountPaid
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g      ON a.ContractServiceSK = g.ContractServiceSK
    INNER JOIN [DWINCMS].[Dim].[CashPoint] e           ON a.CashPointCode = e.CashPointCode
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m            ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n         ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h       ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN ''2025-11-29 00:00:00.000'' AND ''2025-11-29 23:59:59.999''
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService = ''Prepaid''
)
-- pivot per district
, pivoted AS (
    SELECT p.District, ' + @displayCols + ',
           FORMAT(' + @sumExpr + ', ''N0'') AS TotalAmount
    FROM (
        SELECT District, CashPointName, AmountPaid FROM src
    ) s
    PIVOT (
        SUM(AmountPaid) FOR CashPointName IN (' + @cols + ')
    ) p
)
-- totals row computed directly from src (no pivot needed)
, totals AS (
    SELECT 
        ''TOTAL'' AS District,
        ' + @totalColsDisplay + ',
        FORMAT(SUM(AmountPaid), ''N0'') AS TotalAmount
    FROM src
)
-- final derived result (unioned rows) so we can ORDER by on outer select
, final AS (
    SELECT * FROM pivoted
    UNION ALL
    SELECT * FROM totals
)
SELECT *
FROM final
ORDER BY 
    CASE WHEN District = ''TOTAL'' THEN 1 ELSE 0 END,  -- put TOTAL at bottom
    District;
';

-- 4) Execute the dynamic query
EXEC sp_executesql @sql;

-- DAILY SUMMARY FOR POSTPAID MD/NON MD
-- Fixed: District totals for Postpaid MD and Postpaid Non MD (formatted numbers + percentage)
;WITH base AS (
    SELECT 
        n.District,
        g.ContractService,
        a.AmountPaid
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g ON a.ContractServiceSK = g.ContractServiceSK
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN '2025-11-29 00:00:00.000' AND '2025-11-29 23:59:59.999'
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService IN ('Postpaid MD','Postpaid Non MD')
),
agg AS (
    SELECT
        COALESCE(District, 'UNKNOWN') AS District,
        SUM(CASE WHEN ContractService = 'Postpaid MD'     THEN AmountPaid ELSE 0 END) AS Postpaid_MD_Num,
        SUM(CASE WHEN ContractService = 'Postpaid Non MD' THEN AmountPaid ELSE 0 END) AS Postpaid_Non_MD_Num,
        SUM(AmountPaid) AS TotalAmt_Num
    FROM base
    GROUP BY COALESCE(District, 'UNKNOWN')
),
agg_with_grand AS (
    SELECT
        District,
        Postpaid_MD_Num,
        Postpaid_Non_MD_Num,
        TotalAmt_Num,
        SUM(TotalAmt_Num) OVER () AS GrandTotalNum
    FROM agg
),
-- final unioned result placed into one derived set so ORDER BY can safely reference columns
final AS (
    SELECT
        District,
        FORMAT(Postpaid_MD_Num, 'N0')      AS Postpaid_MD,
        FORMAT(Postpaid_Non_MD_Num, 'N0')  AS Postpaid_Non_MD,
        FORMAT(TotalAmt_Num, 'N0')         AS TotalAmount,
        CASE 
            WHEN GrandTotalNum > 0 
            THEN FORMAT( (TotalAmt_Num * 100.0) / GrandTotalNum, 'N2') + '%' 
            ELSE '0.00%' 
        END AS PctOfGrandTotal,
        -- keep numeric total for sorting if needed (not returned in final output but used for ordering if desired)
        TotalAmt_Num AS _SortTotalNum
    FROM agg_with_grand

    UNION ALL

    SELECT
        'TOTAL' AS District,
        FORMAT(SUM(Postpaid_MD_Num), 'N0')     AS Postpaid_MD,
        FORMAT(SUM(Postpaid_Non_MD_Num), 'N0') AS Postpaid_Non_MD,
        FORMAT(SUM(TotalAmt_Num), 'N0')        AS TotalAmount,
        '100.00%'                              AS PctOfGrandTotal,
        SUM(TotalAmt_Num) AS _SortTotalNum
    FROM agg_with_grand
)
SELECT District, Postpaid_MD, Postpaid_Non_MD, TotalAmount, PctOfGrandTotal
FROM final
ORDER BY 
    CASE WHEN District = 'TOTAL' THEN 1 ELSE 0 END,  -- put TOTAL at bottom
    District;



--DAILY CUMMULATIVE SUMMARY TOTAL FOR PPD/PPM WITHOUT CHANNEL NAME
-- Fixed Script 2: District cumulative totals Prepaid, Postpaid MD, Postpaid Non MD
;WITH base AS (
    SELECT 
        n.District,
        g.ContractService,
        a.AmountPaid
    FROM [DWINCMS].[Fact].[PaymentTrans] a
    INNER JOIN [DWINCMS].[Dim].[ContractService] g ON a.ContractServiceSK = g.ContractServiceSK
    LEFT JOIN [DWINCMS].[Dim].[Contracts] m ON a.SPN = m.SPN
    LEFT JOIN [DWINCMS].[Dim].[OrgHierarchy] n ON m.CommOfficeCode = n.CommOfficeCode
    INNER JOIN [DWINCMS].[Dim].[PaymentMethod] h ON a.PaymentMethodSK = h.PaymentMethodSK
    WHERE a.AmountPaid <> 0
      AND a.PaymentDate BETWEEN '2025-11-30 00:00:00.000' AND '2025-11-30 23:59:59.999'
      AND h.PaymentMethodSK IN (35,36)
      AND g.ContractService IN ('Prepaid','Postpaid MD','Postpaid Non MD')
),
agg AS (
    SELECT
        COALESCE(District, 'UNKNOWN') AS District,
        SUM(CASE WHEN ContractService = 'Prepaid'         THEN AmountPaid ELSE 0 END) AS Prepaid_Num,
        SUM(CASE WHEN ContractService = 'Postpaid MD'     THEN AmountPaid ELSE 0 END) AS Postpaid_MD_Num,
        SUM(CASE WHEN ContractService = 'Postpaid Non MD' THEN AmountPaid ELSE 0 END) AS Postpaid_Non_MD_Num,
        SUM(AmountPaid) AS TotalAmt_Num
    FROM base
    GROUP BY COALESCE(District, 'UNKNOWN')
),
agg_with_grand AS (
    SELECT
        District,
        Prepaid_Num,
        Postpaid_MD_Num,
        Postpaid_Non_MD_Num,
        TotalAmt_Num,
        SUM(TotalAmt_Num) OVER () AS GrandTotalNum
    FROM agg
),
final AS (
    -- formatted per-district rows
    SELECT
        District,
        FORMAT(Prepaid_Num, 'N0')         AS Prepaid,
        FORMAT(Postpaid_MD_Num, 'N0')     AS Postpaid_MD,
        FORMAT(Postpaid_Non_MD_Num, 'N0') AS Postpaid_Non_MD,
        FORMAT(TotalAmt_Num, 'N0')        AS TotalAmount,
        CASE 
            WHEN GrandTotalNum > 0 
            THEN FORMAT( (TotalAmt_Num * 100.0) / GrandTotalNum, 'N2') + '%' 
            ELSE '0.00%' 
        END AS PctOfGrandTotal,
        -- keep numeric value for optional sorting (not returned if you don't want)
        TotalAmt_Num AS _SortTotalNum
    FROM agg_with_grand

    UNION ALL

    -- total row
    SELECT
        'TOTAL' AS District,
        FORMAT(SUM(Prepaid_Num), 'N0')         AS Prepaid,
        FORMAT(SUM(Postpaid_MD_Num), 'N0')     AS Postpaid_MD,
        FORMAT(SUM(Postpaid_Non_MD_Num), 'N0') AS Postpaid_Non_MD,
        FORMAT(SUM(TotalAmt_Num), 'N0')        AS TotalAmount,
        '100.00%'                              AS PctOfGrandTotal,
        SUM(TotalAmt_Num) AS _SortTotalNum
    FROM agg_with_grand
)

SELECT District, Prepaid, Postpaid_MD, Postpaid_Non_MD, TotalAmount, PctOfGrandTotal
FROM final
ORDER BY 
    CASE WHEN District = 'TOTAL' THEN 1 ELSE 0 END,  -- put TOTAL at bottom
    District;
