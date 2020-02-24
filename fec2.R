
library(dplyr)
library(stringr)
library(ggplot2)
library(colorspace)
library(scales)
library(lubridate)





fec = read.csv('donations_2019.csv')

fec = fec %>% select(-X)

# # You can't donate a negative amount to a campaign, and I verified that these are not refunds
fec = fec %>% 
  mutate(amount = abs(amount))

head(fec)
summary(fec)

### Total AMOUNT raised by CANDIDATE

candidate_amount = fec %>%
  group_by(candidate) %>%
  summarize(total_raised = sum(amount)/1e6) %>%
  top_n(10) %>% 
  arrange(desc(total_raised)) 

ggplot(candidate_amount, aes(candidate, total_raised)) +
  geom_col(aes(reorder(candidate, -total_raised), fill=candidate), position='dodge') +
  scale_fill_discrete_sequential(palette = "Dark") +
  ggtitle('\nTotal Amount Raised by Candidate', 'FEC Individual Filings 2019') +
  xlab('\nCandidates\n') +
  ylab('Amount Donated') +
  theme(plot.title = element_text(color="grey10", size=18, face="bold"),
        plot.subtitle = element_text(color="grey10", size=12),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="grey10", size=12, angle=90),
        axis.text.y = element_text(color="grey10", size=12),
        axis.title.x = element_text(color="grey10", size=14, face="bold"),
        axis.title.y = element_text(color="grey10", size=14, face="bold"),
        legend.position = 'none') +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "M")) + 
  theme(legend.position="none")



# ### Total DONATIONS by CANDIDATE
# candidate_n_donation = fec %>% 
#   group_by(candidate) %>% 
#   summarize(n_donations = n()) %>% 
#   top_n(10) %>% 
#   arrange(desc(n_donations))
# 
# ggplot(candidate_n_donation, aes(candidate, n_donations)) +
#   geom_col(aes(reorder(candidate, -n_donations), fill=candidate), position='dodge') +
#   scale_fill_discrete_sequential(palette = "Dark") +
#   ggtitle('Total Donations by Candidate', 'FEC Individual Filings 2019') +
#   xlab('\nCandidates\n') +
#   ylab('Donations') +
#   theme(plot.title = element_text(color="grey10", size=18, face="bold"),
#         plot.subtitle = element_text(color="grey10", size=12),
#         axis.ticks = element_blank(),
#         axis.text.x = element_text(color="grey10", size=12, angle=90),
#         axis.text.y = element_text(color="grey10", size=12),
#         axis.title.x = element_text(color="grey10", size=14, face="bold"),
#         axis.title.y = element_text(color="grey10", size=14, face="bold"),
#         legend.position = 'none') +
#   scale_y_continuous(labels = scales::label_comma())

# ### Average DONATIONS Amount by CANDIDATE
# candidate_avg_donation = fec %>% 
#   group_by(candidate) %>% 
#   summarize(total_raised = sum(amount), n_donations = n()) %>% 
#   mutate(avg_donation = total_raised/n_donations) %>% 
#   select(candidate, avg_donation) %>% 
#   top_n(-10) %>% 
#   arrange(avg_donation)
# 
# ggplot(candidate_avg_donation, aes(candidate, avg_donation)) +
#   geom_col(aes(reorder(candidate, avg_donation), fill=candidate), position='dodge') +
#   scale_fill_discrete_sequential(palette = "Dark") +
#   ggtitle('\nAverage Amount per Donation by Candidate', 'FEC Individual Filings 2019') +
#   xlab('\nCandidates\n') +
#   ylab('Average Donation Amount') +
#   theme(plot.title = element_text(color="grey10", size=18, face="bold"),
#         plot.subtitle = element_text(color="grey10", size=12),
#         axis.ticks = element_blank(),
#         axis.text.x = element_text(color="grey10", size=12, angle=90),
#         axis.text.y = element_text(color="grey10", size=12),
#         axis.title.x = element_text(color="grey10", size=14, face="bold"),
#         axis.title.y = element_text(color="grey10", size=14, face="bold"),
#         legend.position = 'none') +
#   scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
#   theme(legend.position="none")



### Total DONORS by CANDIDATE
candidate_n_donor = fec %>% 
  group_by(candidate) %>% 
  summarize(n_donors = n_distinct(donor)) %>%
  top_n(10) %>% 
  arrange(desc(n_donors))

ggplot(candidate_n_donor, aes(candidate, n_donors)) +
  geom_col(aes(reorder(candidate, -n_donors), fill=candidate), position='dodge') +
  scale_fill_discrete_sequential(palette = "Dark") +
  ggtitle('\nTotal Unique Donors by Candidate', 'FEC Individual Filings 2019') +
  xlab('\nCandidates\n') +
  ylab('Unique Donors') +
  theme(plot.title = element_text(color="grey10", size=18, face="bold"),
        plot.subtitle = element_text(color="grey10", size=12),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="grey10", size=12, angle=90),
        axis.text.y = element_text(color="grey10", size=12),
        axis.title.x = element_text(color="grey10", size=14, face="bold"),
        axis.title.y = element_text(color="grey10", size=14, face="bold"),
        legend.position = 'none') +
  scale_y_continuous(labels = scales::label_comma())



### Average AMOUNT/DONOR by CANDIDATE Largest
candidate_avg_donor = fec %>% 
  filter(candidate == 'Donald Trump' | candidate == 'Bernie Sanders' | candidate == 'Elizabeth Warren' | 
           candidate == 'Pete Buttigieg' | candidate == 'Joe Biden' | candidate == 'Kamala Harris' | 
           candidate == 'Andrew Yang' | candidate == 'Amy Klobuchar' | candidate == 'Cory Booker' | 
           candidate == "Beto O'Rouke") %>% 
  group_by(candidate) %>% 
  summarize(total_raised = sum(amount), n_donors = n_distinct(donor)) %>% 
  mutate(avg_donor_donation = total_raised/n_donors) %>% 
  select(candidate, avg_donor_donation) %>% 
  top_n(-10) %>% 
  arrange(desc(avg_donor_donation))

ggplot(candidate_avg_donor, aes(candidate, avg_donor_donation)) +
  geom_col(aes(reorder(candidate, avg_donor_donation), fill=candidate), position='dodge') +
  scale_fill_discrete_sequential(palette = "Dark") +
  ggtitle('\nAverage Amount per Donor by Candidate', 'FEC Individual Filings 2019') +
  xlab('\nCandidates\n') +
  ylab('Average Donor Amount') +
  theme(plot.title = element_text(color="grey10", size=18, face="bold"),
        plot.subtitle = element_text(color="grey10", size=12),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="grey10", size=12, angle=90),
        axis.text.y = element_text(color="grey10", size=12),
        axis.title.x = element_text(color="grey10", size=14, face="bold"),
        axis.title.y = element_text(color="grey10", size=14, face="bold"),
        legend.position = 'none') +
  scale_y_continuous(labels = scales::dollar_format(prefix="$"))





# Daily Amount
daily_amount = fec %>% 
  # filter(candidate == 'Donald Trump' | candidate == 'Bernie Sanders' | candidate == 'Elizabeth Warren' |
  # candidate == 'Pete Buttigieg' | candidate == 'Joe Biden' | candidate == 'Kamala Harris' |
  # candidate == 'Andrew Yang' | candidate == 'Amy Klobuchar' | candidate == 'Cory Booker' |
  # candidate == "Beto O'Rouke") %>%
  # filter(candidate == 'Bernie Sanders' | candidate == 'Elizabeth Warren' |
  #          candidate == 'Pete Buttigieg' | candidate == 'Joe Biden' | candidate == 'Kamala Harris' |
  #          candidate == 'Andrew Yang' | candidate == 'Amy Klobuchar' | candidate == 'Cory Booker' |
  #          candidate == "Beto O'Rouke") %>%
  filter(candidate=='Pete Buttigieg') %>%
  group_by(candidate, date = as.Date(date)) %>%
  #filter(date>='2019/06/25' & date<'2019/07/02') %>% 
  summarise(daily_amount_sum = sum(amount)/1e3)

ggplot(daily_amount, aes(x=date, y=daily_amount_sum, group=candidate)) +
  geom_line(aes(color=candidate)) +
  ggtitle('\nDaily Amount Raised by Pete Buttigieg', 'FEC Individual Filings 2019') +
  geom_line(aes(color=candidate), size=1) +
  xlab('\nDate\n') +
  ylab('\nAmount Raised\n') +
  theme(plot.title = element_text(color="grey10", size=18, face="bold"),
        plot.subtitle = element_text(color="grey10", size=12),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="grey10", size=12),
        axis.text.y = element_text(color="grey10", size=12),
        axis.title.x = element_text(color="grey10", size=14, face="bold"),
        axis.title.y = element_text(color="grey10", size=14, face="bold"),
        legend.title = element_text(color="grey10", size=14, face="bold"),
        legend.text = element_text(color="grey10", size=12)) +
  scale_x_date(date_minor_breaks='2 weeks') +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "K")) +
  scale_color_manual(name="Candidate", values=c("#002649", "#8E1600"))
  #scale_color_discrete(name="Candidate")







# Daily Donors
daily_amount = fec %>% 
  #filter(candidate == 'Donald Trump' | candidate == 'Bernie Sanders' | candidate == 'Elizabeth Warren' | 
          # candidate == 'Pete Buttigieg' | candidate == 'Joe Biden' | candidate == 'Kamala Harris' | 
          # candidate == 'Andrew Yang' | candidate == 'Amy Klobuchar' | candidate == 'Cory Booker' | 
          # candidate == "Beto O'Rouke") %>% 
  filter(candidate == 'Bernie Sanders' | 
           candidate == 'Elizabeth Warren' |
           candidate == 'Pete Buttigieg' | 
           candidate == 'Joe Biden' | 
           candidate == 'Kamala Harris' |
           #candidate == 'Andrew Yang' | 
           #candidate == 'Amy Klobuchar' | 
           #candidate == 'Cory Booker' |  
           candidate == "Beto O'Rouke") %>%
  #filter(candidate=='Pete Buttigieg') %>%
  group_by(candidate, date = as.Date(date)) %>%
  filter(date>='2019/06/25' & date<'2019/07/02') %>% 
  summarise(amount_daily = sum(amount)/1e3)

ggplot(daily_amount, aes(x=date, y=amount_daily, group=candidate)) +
  geom_line(aes(color=candidate)) +
  ggtitle('\nAmount Raised, 7 Days Trailing First Debate', 'FEC Individual Filings 2019') +
  geom_line(aes(color=candidate), size=1.5) +
  xlab('\nDate\n') +
  ylab('\nAmount Raised\n') +
  theme(plot.title = element_text(color="grey10", size=18, face="bold"),
        plot.subtitle = element_text(color="grey10", size=12),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="grey10", size=12),
        axis.text.y = element_text(color="grey10", size=12),
        axis.title.x = element_text(color="grey10", size=14, face="bold"),
        axis.title.y = element_text(color="grey10", size=14, face="bold"),
        legend.title = element_text(color="grey10", size=14, face="bold"),
        legend.text = element_text(color="grey10", size=12)) +
  scale_x_date(date_minor_breaks='2 weeks') +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "K")) +
  #scale_color_manual(name="Candidate", values=c("#002649", "#8E1600"))
  scale_color_manual(name="Candidate", values=c('#0000ff', '#007000', '#6f0070', '#0092ff', '#cc7a00'))












