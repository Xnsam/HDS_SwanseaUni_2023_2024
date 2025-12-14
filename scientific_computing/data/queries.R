# Header -----------------------------------------------------------------------
# File name: queries.R
# Folder/Module name: data
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-25
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing queries
# ------------------------------------------------------------------------------


prev_table_view <- "
CREATE OR REPLACE VIEW  prev_tbl_view
AS
SELECT    practice_tbl.indicator,
          practice_tbl.patient_count,
          practice_tbl.denominator,
          practice_tbl.practiceid,
          100 * (practice_tbl.denominator  / 
                 practice_tbl.patient_count) AS prevalence
FROM     (
          SELECT    CAST(qa.field4 AS float) AS denominator,
                    CAST(patient_count_tbl.patient_count AS FLOAT),
                    patient_count_tbl.practiceid,
                    qa.indicator
          FROM     (
                    SELECT    orgcode AS practiceid,
                              SUM(field4) AS patient_count
                    FROM      qof_achievement
                    GROUP BY  orgcode
                   ) AS patient_count_tbl
          INNER JOIN  
                qof_achievement qa 
          ON    patient_count_tbl.practiceid = qa.orgcode
          ) AS practice_tbl
"

address_county_view <- "
CREATE OR REPLACE VIEW     address_county_view 
AS              
SELECT    * 
FROM      address a 
WHERE     county is not null and length(county) > 0 AND county != ' ';
"

qof_practiceid_view <- "
CREATE OR REPLACE VIEW    qof_practiceid_view 
AS             
SELECT    DISTINCT(qa.orgcode) 
FROM      qof_achievement qa;
"

base_table_view <- "
CREATE OR REPLACE VIEW    base_table_view 
AS             
SELECT   *
FROM     public.base_table_view0
WHERE    net_cost != 0;
"

base_table_view0 <- "
CREATE OR REPLACE VIEW    base_table_view0
AS             
SELECT    gdut.hb AS hb_name,
          gdut.practiceid AS practice_id,
          gdut.nic AS net_cost,
          gdut.actcost AS actual_cost,
          gdut.bnfcode,
          ROUND(
                CAST((gdut.nic - gdut.actcost) / gdut.nic AS NUMERIC),
                5) * 100 AS perc_discount
FROM       gp_data_up_to_2015 AS gdut
WHERE     CAST(period / 100 as integer) = 2015 AND
          gdut.practiceid IN (
                              SELECT    * 
                              FROM      public.qof_practiceid_view
                              );
"

cat_table_view <- "
CREATE OR REPLACE VIEW cat_table_view
AS 
SELECT    net_actcost_tbl.practice_id,
          net_actcost_tbl.bnfcode,
          net_actcost_tbl.net_cost,
          net_actcost_tbl.actual_cost,
          b.chapterdesc as category,
          b.sectiondesc as sub_category
FROM     (
          SELECT    *
          FROM      (SELECT    *
                     FROM      public.base_table_view) AS base_table
                     WHERE
                          base_table.actual_cost > base_table.net_cost
                     ) AS net_actcost_tbl
INNER JOIN bnf AS b
ON       b.bnfchemical = LEFT(net_actcost_tbl.bnfcode, 9);
"