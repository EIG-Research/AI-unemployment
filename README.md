# All Quiet on the AI Front 

This repository contains the data and code to generate estimates in "All Quiet on the AI Front" analysis [link], and the accompanying Agglomerations post [link]. Accurate as of time of publication, [date of publication]. Contact nathan@eig.org with any questions.

To run, download the data from the accompanying data from [dropbox](https://www.dropbox.com/scl/fo/34xtxjwatomz5vlgzwj5j/AJSUajyhLOZgaXk5I32TJoM?rlkey=9wxaewm3whxmtsqmxs36sqslo&st=lbt2r4ml&dl=0) into the /data/1raw folder. Intermediate data will be stored in /data/2wrangled, and final charts will be stored in /data/3final.

----------

## Data

### Microdata
1. Current Population Survey (CPS) monthy samples January 2015 - June 2025, downloaded from [IPUMS](https://cps.ipums.org/cps/) FILE NAME: <b>cps_basic_monthly.dta.gz</b>
2. CPS Annual Social and Economic Supplement (ASEC) samples March 2015 - March 2024, downloaded from [IPUMS](https://cps.ipums.org/cps/). FILE NAME: <b>cps_asec.dta.gz</b>
3. CPS Outgoing Rotational Group (ORG) samples 2015 - 2024, downloaded from the [Economic Policy Institute](https://microdata.epi.org/). FILE NAME: <b>epi_cpsorg_1979_2025</b>

### Crosswalks
5. 2010 SOC to 2018 SOC Crosswalk, downloaded from the [Bureau of Labor Statistics (BLS)](https://www.bls.gov/soc/2018/soc_2010_to_2018_crosswalk.xlsx). FILE NAME: <b>soc_2010_to_2018_crosswalk.xlsx</b>
6. Census SOC to Census occupation code crosswalk, downloaded from [Census](https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html). FILE NAME: <b>2018-occupation-code-list-and-crosswalk.xlsx</b>
7. [O*NET](https://www.onetcenter.org/db_releases.html) datasets 25.0, and 25.1. FILE NAMES: <b>db_25_0_excel.zip</b>, and <b>db_25_1_excel.zip</b>
8. David Dorn to 1990 Census crosswalk, downloaded from [Dorn's website](https://www.ddorn.net/data.htm). FILE NAME:  <b>occ1990_occ1990dd.zip</b>

### AI Exposure measures
8. Felten, E., Raj, M., & Seamans, R. (2021). Occupational, industry, and geographic exposure to artificial intelligence: A novel dataset and its potential uses. Strategic Management Journal, 42(12), 2195–2217. [https://doi.org/10.1002/smj.3286](https://doi.org/10.1002/smj.3286). Downloaded from the accompanying [GitHub](https://github.com/AIOE-Data/AIOE). FILE NAME: <b>AIOE_DataAppendix.xlsx</b>
9. Tyna Eloundou et al., GPTs are GPTs: Labor market impact potential of LLMs. Science 384,1306-1308 (2024). [https://arxiv.org/abs/2303.10130](https://arxiv.org/abs/2303.10130). Downloaded from the accompanying [GitHub](https://github.com/openai/GPTs-are-GPTs). FILE NAME: <b>gptsRgpts_occ_lvl.csv](gptsRgpts_occ_lvl.csv</b>
10. The Labor Impact of Generative AI on Firm Values (Eisfeldt, Schubert, Taska, Zhang, 2024).[https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4436627](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4436627). Downloaded from the accompanying [GitHub](https://github.com/gschubert/website/blob/gh-pages/genaiexp_estz_occscores.csv). FILE NAME: <b>genaiexp_estz_occscores.csv</b>
11. Webb (2022). The Impact of Artificial Intelligence on the Labor Market [https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3482150](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3482150). Downloaded from the accompanying [site](https://www.notion.so/michaelwebb/Data-for-The-Impact-of-Artificial-Intelligence-on-the-Labor-Market-3b52b281505a48b8be107d11d8d0c363). FILE NAME: <b>exposure_by_occ1990dd_lswt2010.csv</b>

----------

## Code

1. <b>01 Crosswalks.R</b> Constructs crosswalks and the primary AI exposure measures from Felten et al.
2. <b>02 Microdata Monthly Build.R</b> Constructs primary datasets for analysis (cps_monthly_w_xposure_xwalked.dta, cps_asec_w_xposure_xwalked.dta, and cps_org_w_xposure_xwalked.dta)
3. <b>03 Main Analysis.R</b> Creates charts and statistics cited in the main text.
4. <b>04 Appendix.R</b> Creates charts and statistics cited in the accompanying appendix.

