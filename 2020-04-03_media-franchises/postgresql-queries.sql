--Part 1

--Obtain the ordering of the most profitable revenue categories for each franchise
with franchise_table as (
    select franchise
    , original_media
    , creators
    , owners
    , revenue_category
    , revenue
    , row_number() over (partition by franchise
                         , original_media
                         , creators
                         , owners
                          order by revenue desc) as row_num
from tidytuesday.media_franchises
    order by 1, 2, 3, 4
)
--Getting the most profitable revenue category
, profitable_revenue_category as (
    select ft.franchise
    , ft.original_media
    , ft.creators
    , ft.owners
    , ft.revenue_category
from franchise_table ft
where ft.row_num = 1
)
--Count the number of categories and the sum of total revenue for each franchise
, revenue_table as (
select ft.franchise
    , ft.original_media
    , ft.creators
    , ft.owners
    , count(*) as num_revenue_categories
    , sum(ft.revenue) as total_revenue
from franchise_table ft
group by 1, 2, 3, 4
)
--Join all the data together
select rt.*
    , prc.revenue_category as most_profitable_revenue_category
from revenue_table rt
inner join profitable_revenue_category prc 
    on prc.franchise = rt.franchise
    and prc.original_media = rt.original_media
    and prc.creators = rt.creators
    and prc.owners = rt.owners
order by 1, 2, 3, 4

--Part 2

--Companies that own atleast three franchises

--select * from tidytuesday.media_franchises;

with temp_table as (
select owners
	, franchise
	, original_media
	, sum(revenue) as total_revenue
from tidytuesday.media_franchises
-- List of owners who have at least 3 franchises
where owners in (
select owners
from tidytuesday.media_franchises
group by 1
having count(distinct franchise) > 2
	)
group by 1, 2, 3
order by 1, 2, 3
)
, ranked_franchises as (
select owners
	, franchise
	, original_media
	, total_revenue
	, rank () over (partition by owners order by total_revenue desc)
from temp_table
)
select owners
	, franchise
	, original_media
	, total_revenue
from ranked_franchises
where rank = 1
order by total_revenue desc;


-- PART 3

select * 
from tidytuesday.media_franchises;


select distinct original_media
from tidytuesday.media_franchises;

with temp_table as (
	select original_media
	, revenue_category
	, sum(revenue) as total_revenue_billion_usd
from tidytuesday.media_franchises
group by 1, 2
order by 1 asc, 3 desc
)
, ranked_table as (
	select original_media
	, revenue_category
	, total_revenue_billion_usd
	, dense_rank() over (partition by original_media 
					   order by total_revenue_billion_usd desc)
from temp_table
)
select original_media
	, total_revenue_billion_usd
from ranked_table
where dense_rank = 1
and revenue_category = 'Merchandise, Licensing & Retail'
order by 2 desc;
