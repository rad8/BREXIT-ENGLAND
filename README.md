# BREXIT-ENGLAND
Who are the Leave Voters?
A Quantitative Data Analysis of the 2016 EU Referendum

1.	Introduction

A referendum was held on June 23, 2016 on the UK’s membership of the European Union which resulted in the UK electorate voting to leave the EU by 51.9% to 48.1%. Despite many of the polls in the run-up to the referendum that predicted a win for remain (NatCen Social Research 2016), vote leave won by a margin of 3.78%. Why did the British vote to leave the EU? Since the referendum, a number of articles and research papers have been published seeking to explain the leave vote. Specifically, Becker, S., et.al (2017) revealed that the social and economic characteristics of the population explained the leave vote more than exposure to the EU either through migration or trade. In this paper, I will use aggregate level quantitative data analysis techniques in order to support their view. Hence, the aim of this paper is to identify those variables that most explain the leave vote by building a predictor model. However, the reader should bear in mind that this analysis is based on only England’s Local authority data and does not represent Northern Ireland, Scotland or Wales. It is also difficult to make inference to the UK wide population as the data is not randomly selected.

Data Scource: 

EU refurrendum result from The Electoral Commission https://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/past-elections-and-referendums/eu-referendum/electorate-and-count-information 

NOMIS: Official Labour Market Statistics https://www.nomisweb.co.uk/

Office for National Statistics: https://www.ons.gov.uk/

Softwares Used: open source SQLite and R

Variables

Variables	Variable Type	Explanation	Source
Region	Foreign Key	Region where local authority is located	
LA Name	Foreign key	Name of the local Authority	
LA_code 14
	Primary Key	Local authority Area code before April 2015	EU-referendum result 
The Electoral Commission
Pct_Leave	Dependent	Percentage of Leave Outcome	“                     “
Diff	Dependent	Leave percentage – Remain Percentage	“                     “
Outcome	Dependent	Whether ‘Leave’ or ‘Remain’	“                     “
pop_2011
	Base population	Population according to 2011 Census 	Official Labour Market Statistic: NOMIS
Young_Age	Independent	Aggregate of the age groups between 16 to 34	“                     “
middle_age
	Independent	Aggregate of the age groups between 35 to 49	“                     “
Older_age	Independent	Age group 50+	“                     “
hh_deprived
	Independent	Aggregate of deprived households in one or more dimensions	“                     “
Claimant	Independent	count of Universal Credit and Job Seekers Allowance claimants	“                     “
Economically active
	
Base population	16 and over years olds who are active participant in the labour market	“                     “
Economically in active
	Independent	16 and over years olds who are not active participant in the labour market	“                     “

ethn_white
	Independent	Usual residents who are white	“                     “
Jobsdensity
	Independent	The numbers of jobs per resident aged 16-64, includes employees, self-employed, government-supported trainees and HM Forces.	“                     “
White_migrant
	Independent	White ethnic group migrants living in an area in the year preceding the census	“                     “
level4andabove
	Independent	Stock of usual residents over the age of 16 with level 4 and above qualification	“                     “
noqualifications
	Independent	Stock of usual residents over the age of 16 with no qualification	“                     “
abhigher_interm
	Independent	Approximation of social grade of residents aged 16 to 64 living in a household with managerial/administrative or professional occupation	“                     “
semi_unskilled
	Independent	Approximation of social grade of residents aged 16 to 64 living in households who are lowest grade and unemployed	“                     “
prop_Owned	Independent	Total households who own the occupied accomadation: (outright, mortgage and shared ownership)	“                     “
socialrented
	Independent	Total households who rent the occupied accommodation from council and other social rent	“                     “
Crime
	Independent	Recorded crime by community safety partnership year ending March 2016.	Office for National statistics
life_Sat
	Independent	Mean overall satisfaction with one’s life ranked from 0(no satisfaction) to 10(completely satisfied). 	Office for National Statistics: Estimates of Personal well-being from the Annual Population Survey 2015/16
disp_inc2016
	Independent	Gross disposable household income per head,  2016	Office for National statistics
