-- T-SQL to return one row per horizon for all horizons of the dominant component of queried mukeys (inputed to '%s')

-- https://sdmdataaccess.nrcs.usda.gov/Query.aspx
-- Report horizon-level values for all COKEYs of specified MUKEYs
-- code chunk for dominant cokeys: https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=SDA-QueryParts2 "Select Dominant Component"
-- code chunk for depth to bedrock: https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=SDA-DepthToMinBedrockbyMapunit
-- code chunk for depth to any restriction and to deepest horizon: https://github.com/ncss-tech/SoilDataDevelopmentToolbox/blob/master/SDA_SpatialQuery_Custom.py
-- code chunk for summed fragment volume per horizon: soilDB::get_chorizon_from_SDA()
-- code chunk for concatenated texture values per horizon (there can be none, one, or multiple): https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=SDA-QueryParts2 "List Interpretation Reasons"

-- Remove condition `component.majcompflag = 'Yes'` for STATSGO queries because not every STASTGO mukey has at least one component with majcompflag = 'Yes'
-- `legend.musym` exists for SSURGO but not for STATSGO


SELECT
  legend.areasymbol,
  mapunit.mukey AS MUKEY,
  component.cokey AS COKEY,
  component.comppct_r,
  component.taxorder, component.taxsubgrp,
  component.compname, component.compkind,
  SUBSTRING(
    (
      SELECT '; ' + chtxg_tx.texture
      FROM chorizon AS ch_txg
      LEFT OUTER JOIN chtexturegrp AS chtxg_tx ON ch_txg.chkey = chtxg_tx.chkey AND (chtxg_tx.rvindicator = 'Yes' OR chtxg_tx.rvindicator IS NULL)
      WHERE chorizon.chkey = chtxg_tx.chkey
      FOR XML PATH('')
    ),
    3, 20
  ) AS texture,
  SUBSTRING(
    (
      SELECT '; ' + chtx_tx.lieutex
      FROM chorizon AS ch_tx
      LEFT OUTER JOIN chtexturegrp AS chtxg_tx ON ch_tx.chkey = chtxg_tx.chkey AND (chtxg_tx.rvindicator = 'Yes' OR chtxg_tx.rvindicator IS NULL)
      LEFT OUTER JOIN chtexture AS chtx_tx ON chtxg_tx.chtgkey = chtx_tx.chtgkey
      WHERE chorizon.chkey = chtxg_tx.chkey
      FOR XML PATH('')
    ),
    3, 200
  ) AS lieutex,
  chorizon.desgnmaster,
  RANK() OVER (PARTITION BY component.cokey ORDER BY chorizon.hzdepb_r) AS Horizon_No,
  MAX(chorizon.hzdepb_r) OVER (PARTITION BY component.cokey) AS Horizon_depth,
  chorizon.hzdept_r, chorizon.hzdepb_r,
  chorizon.dbovendry_r,
  chorizon.sandtotal_r, chorizon.silttotal_r, chorizon.claytotal_r,
  chorizon.wsatiated_r, chorizon.om_r, chorizon.ec_r, chorizon.ph1to1h2o_r,
  (
    SELECT SUM(fragvol_r)
    FROM chorizon AS ch_fv
    LEFT OUTER JOIN chfrags AS chf_fv ON chf_fv.chkey = ch_fv.chkey
    WHERE chorizon.chkey = ch_fv.chkey
  ) AS fragvol_r,
  (
    SELECT CAST(MIN(resdept_r) AS INT)
    FROM component AS co_rd2
    LEFT OUTER JOIN corestrictions AS cr_rd2 ON co_rd2.cokey = cr_rd2.cokey
    WHERE component.cokey = co_rd2.cokey AND reskind IN ('Lithic bedrock', 'Paralithic bedrock', 'Densic bedrock', 'Fragipan', 'Duripan', 'Sulfuric')
  ) AS RootZoneRestriction_depth,
  (
    SELECT CAST(MIN(resdept_r) AS INT)
    FROM component AS co_rd3
    LEFT OUTER JOIN corestrictions AS cr_rd3 ON co_rd3.cokey = cr_rd3.cokey
    WHERE component.cokey = co_rd3.cokey AND reskind LIKE '%bedrock%'
  ) AS Bedrock_depth


FROM legend

INNER JOIN mapunit ON legend.lkey = mapunit.lkey

INNER JOIN component ON mapunit.mukey = component.mukey AND component.majcompflag = 'Yes'

LEFT OUTER JOIN chorizon ON component.cokey = chorizon.cokey

WHERE
  mapunit.mukey IN (%s)
AND
  component.cokey IN (
    SELECT TOP 1 co_dc.cokey
    FROM component AS co_dc
    INNER JOIN mapunit AS mu_dc ON co_dc.mukey = mu_dc.mukey and mu_dc.mukey = mapunit.mukey
    ORDER BY co_dc.comppct_r DESC
  )

ORDER BY mapunit.mukey, component.comppct_r DESC, component.compname, chorizon.hzdept_r ASC
