library(shiny)


# Template ----------------------------------------------------------------

# tags$li(
#   p(
#     strong(
#       ""
#     ),
#     span(" ,  - "
#     ),
#     a(
#       "",
#       targe = "_blank",
#       href = ""
#     )
#   )
# )


# Start from the most recent ----------------------------------------------


articles <- div(
  
  class = "pub",
  
  h1("Selected list of news items featuring the UK Housing Observatory team:"),
  br(),
  br(),
  
  tags$ul(
    tags$li(
      p(
        strong(
          "The Independent"
        ),
        span(
          ", 6 April 2018 - "
        ),
        a(
          "Finally got that last home? Don't expect the same wealth as your parents", 
          target = "_blank",
          href = "https://www.independent.co.uk/money/spend-save/new-homebuyers-generation-property-ladder-personal-finances-a8290391.html")
      )
    ),
    
    tags$li(
      p(
        strong(
          "The Times"
        ),
        span(" , 11 April 2017 - "
        ),
        a(
          "The problem is not housing supply - it's our boom-and-bust mentality",
          targe = "_blank",
          href = "https://www.thetimes.co.uk/article/the-problem-is-not-housing-supply-its-our-boom-and-bust-mentality-dpl6gtn3w"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Financial Times"
        ),
        span(" , 11 April 2017 - "
        ),
        a(
          "Rising house prices lift birth rates for homeowners, study shows",
          targe = "_blank",
          href = "https://www.ft.com/content/0ead7b42-1ec0-11e7-a454-ab04428977f9"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Business Insider"
        ),
        span(" , 11 April 2017 - "
        ),
        a(
          "The key driver of Britain's wildly inflated property market might not be what we think it is",
          targe = "_blank",
          href = "http://uk.businessinsider.com/britains-housing-market-is-not-being-driven-by-a-supply-and-demand-imbalance-2017-4"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Royal Economic Society"
        ),
        span(" , Media Briefing, April 2017 - "
        ),
        a(
          "Exuberance caused the UK house price boom",
          targe = "_blank",
          href = "http://www.res.org.uk/details/mediabrief/10502534/EXUBERANCE-CAUSED-THE-UK-HOUSE-PRICE-BOOM.html?platform=hootsuite"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "MSN"
        ),
        span(" , 10 April 2017 - "
        ),
        a(
          "The Only Solution to Britain's Housing Crisis May Be a Crash",
          targe = "_blank",
          href = "https://www.msn.com/en-gb/money/homeandproperty/the-only-solution-to-britains-housing-crisis-may-be-a-crash/ar-BBzHraS?li=AA54rU"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Finance Monthly"
        ),
        span(", 14 March 2017 - "
        ),
        a(
          " Your thoughts: House Price Growth Slowing Down",
          targe = "_blank",
          href = "https://www.finance-monthly.com/2017/03/your-thoughts-house-price-growth-slowing-down/"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Property Reporter"
        ),
        span(" (Web), 8 February 2017 "
        ),
        a(
          "Economists predict housing crash won't happen in 2017",
          targe = "_blank",
          href = "http://www.propertyreporter.co.uk/property/economists-predict-housing-crash-wont-happen-in-2017.html"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Property Wire"
        ),
        span(",  8 February 2017 - "
        ),
        a(
          "House prices set to grow by 3.5% in the UK in 2017",
          targe = "_blank",
          href = "http://www.propertywire.com/news/uk/house-prices-set-grow-3-5-uk-2017/"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Property Wire"
        ),
        span(", 18 April 2016 - "
        ),
        a(
          "The growth of London house prices has slowed down, new data suggests",
          targe = "_blank",
          href = "http://www.propertywire.com/news/europe/london-house-price-growth-2/"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "World Finance"
        ),
        span(", 18 January 2016 - "
        ),
        a(
          "Will there be another property bubble in 2016?",
          targe = "_blank",
          href = "http://www.worldfinance.com/infrastructure-investment/will-there-be-another-property-bubble-in-2016"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Financial Times"
        ),
        span(", November 6, 2015 - "
        ),
        a(
          "London property bubble risks are overblown, say agents",
          targe = "_blank",
          href = "https://www.ft.com/content/d364168a-847a-11e5-8095-ed1a37d1e096"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Mirror"
        ),
        span(", November 3, 2015 - "
        ),
        a(
          " London house prices set to crash in 2017 with market ‘currently nearing a bubble’",
          targe = "_blank",
          href = "http://www.mirror.co.uk/money/london-house-prices-set-crash-6756976"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "This is Money"
        ),
        span(", November 3, 2015 - "
        ),
        a(
          " Britain's property market isn't in a bubble, academics claim - but London is on the cusp of one...and it could blow up in 2017",
          targe = "_blank",
          href = "http://www.thisismoney.co.uk/money/mortgageshome/article-3300549/UK-property-market-isn-t-bubble-academics-claim-London-cusp.html"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "Evening Standard"
        ),
        span(", November 2, 2015 - "
        ),
        a(
          "Is London heading for another property price bubble as soon as 2017?",
          targe = "_blank",
          href = "http://www.standard.co.uk/news/london/london-faces-risk-of-housing-bubble-by-2017-report-warns-a3104941.html"
        )
      )
    ),
    
    tags$li(
      p(
        strong(
          "The Conversation"
        ),
        span(", June 11, 2014 - "
        ),
        a(
          "Explosive London Housing bubble will spread to the rest of the UK",
          targe = "_blank",
          href = "http://theconversation.com/explosive-london-housing-bubble-will-spread-to-the-rest-of-the-uk-27721"
        )
      )
    )
  )
  
  
  
  
  
)

