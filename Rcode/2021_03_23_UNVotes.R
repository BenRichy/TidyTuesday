library(tidytuesdayR)
library(tidyverse)
library(countrycode)
library(maps)
library(ggthemes)

# load data
tuesdata <- tidytuesdayR::tt_load('2021-03-23')

unVotes <- tuesdata$unvotes
rollCall <- tuesdata$roll_calls
issues <- tuesdata$issues

# join UN data sets together and filter for last 10 years only and for where there are issues
joinedUnVotes <- unVotes %>% 
  left_join(rollCall, by = "rcid") %>% 
  left_join(issues, by = c("rcid")) %>% 
  select(-c("amend", "para")) %>% 
  mutate(continent = countrycode(country_code, "iso2c", "region"),
         voteStatus = 1) %>% 
  filter(!is.na(short_name),
         !is.na(issue),
         session >= 65)


# calculate for each UN member:
# total votes for each issue
# total votes for yes, no, abstain for each issue
# % of votes for yes, no, abstain for each issue
# filter to show only the nos
# take only the top issue for each country
countryVotes <- joinedUnVotes %>% 
  select(c("country",
         "country_code",
         "short_name",
         "issue",
         "vote",
         "voteStatus")) %>%
  group_by(country,
             country_code,
             short_name,
             issue,
             vote) %>% 
  mutate(voteCount = sum(voteStatus)) %>%
  unique() %>% 
  ungroup() %>% 
  group_by(country,
           country_code,
           short_name,
           issue) %>% 
  mutate(TotalvoteCount = sum(voteCount),
         VotePercent = voteCount/TotalvoteCount) %>% 
  filter(vote == "no") %>% 
  ungroup() %>% 
  group_by(country,
           country_code) %>% 
  slice_max(order_by = VotePercent)

# fix country code for Namibia since it appears as an NA error
# And for Serbia, since it is registered in the UN as Yugoslavia
countryVotes$country_code[countryVotes$country == "Namibia"] <- "NA"
countryVotes$country_code[countryVotes$country == "Yugoslavia"] <- "RS"



# plot

# load in world map data and clean map names
world_map <- map_data("world")
countryCodes <- iso3166 %>% 
  mutate(mapname = case_when(a2 == "NO" ~ "Norway",
                             a2 == "GB" ~ "UK",
                             a2 == "CN" ~ "China",
                             a2 == "FI" ~ "Finland",
                             TRUE ~ as.character(mapname)))

# join vote data to map data and factorise the issues
country_Votes_map <- world_map %>% 
  left_join(countryCodes, by = c("region" = "mapname")) %>% 
  left_join(countryVotes, by = c("a2" = "country_code")) %>% 
  mutate(issue = case_when(is.na(issue) ~ "Data not available",
                           TRUE ~ as.character(issue)),
         issue = factor(issue, levels = c("Arms control and disarmament",
                                          "Colonialism",
                                          "Economic development",
                                          "Human rights",
                                          "Nuclear weapons and nuclear material",
                                          "Palestinian conflict",
                                          "Data not available"))) 



# make plot
ggplot(data = country_Votes_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = issue ), color = "grey50") +
  theme_void() +
  scale_fill_pander(na.value = "grey50") +
  labs(fill = "UN World Issue",
       title = "Members of the UN and the world issues that they proportionally vote against the most (2010 - 2020)",
       caption = str_wrap("Note: A 'vote against' is not necessarily disagreeing with the issue, for example, voting no to nuclear issues could be voting no to nuclear disarmement.\n
                          Data based on: Erik Voeten 'Data and Analyses of Voting in the UN General Assembly' Routledge Handbook of International Organization, edited by Bob Reinalda (published May 27, 2013). Available at SSRN: http://ssrn.com/abstract=2111149", 153)) +
  theme(plot.background = element_rect(fill = "#B8EDFD"),
        plot.title = element_text(margin = margin(t=10)),
        plot.margin = unit(c(0,0,0.2,0.5), "cm"),
        legend.margin = margin(r=10))
  
ggsave("plots/2021_03_23_UNVotes.png",height = 7,width=12)



