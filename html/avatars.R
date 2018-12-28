# Set up in header shiny light-blue = rgb(239,240,241)

avatar_box <- function(src_img, name, title = "Researcher", mail, 
                       has_website = FALSE, website = NULL, box_width = 3) {
  box(width = box_width, background = "light-blue",
      align = "center",
      img(src = src_img, 
          style = "object-fit: cover;object-position: 100% 0%;
          border-radius:50%;width: 150px;height: 150px;"),
      if (has_website) {
        a(
          h3(name,
             style = "color: #000000;margin: 20px 0 10px 0;"), #font-size: 2.4em;font-weight: 300
          href = website, 
          target = "_blank", 
          rel = "noopener noreferrer" )
      }else{
        h3(name)
      }
      ,
      h4(title, 
         style = "color: rgba(0,0,0, 0.54);margin: 0px 0 10px 0;"), #font-size: 2em;font-weight: 300;
      a(mail, 
        style = "color:#b5121b;",
        href = paste0("mailto:", website),
        style = "font-size:1.4em;target:_blank;rel=noopener;"),
      br()
  )
}


# Entries -----------------------------------------------------------------

avatars <- div(
  
  # First Row
  fluidRow(
    column(width = 12,
           fluidRow(style = "background-color: rgb(239,240,241);",
                    
                    br(),
                    h2("Meet the Team", style = "text-align: center;"),
                    h4("All members in the observatory are researchers", 
                       style = "text-align: center;"),
                    br(),
                    
                    avatar_box(src_img = "themis.jpg", 
                               name = "Themis Pavlidis", 
                               title = "Director",
                               mail = "e.pavlidis@lancaster.ac.uk",
                               has_website = TRUE, 
                               website = "https://sites.google.com/site/etpavlidis/"),
                    avatar_box(src_img = "ivan.jpg", 
                               name = "Ivan Paya", 
                               title = "Director",
                               mail = "i.paya@lancaster.ac.uk",
                               has_website = TRUE, 
                               website = "https://ivanpaya.weebly.com/"),
                    avatar_box(src_img = "alisa.jpg", 
                               name = "Alisa Yusupova", 
                               title = "Web Administator",
                               mail = "a.yusupova@lancaster.ac.uk",
                               has_website = TRUE, 
                               website = "http://www.lancaster.ac.uk/lums/people/alisa-yusupova"),
                    avatar_box(src_img = "kostas.jpeg", 
                               name = "Kostas Vasilopoulos", 
                               title = "Web Administrator",
                               mail = "k.vasilopoulos@lancaster.ac.uk",
                               has_website = TRUE, 
                               website = "https://kvasilopoulos.netlify.com/")
                    )
           )
    ),
  
  # Second Row
  fluidRow( 
    # column(width = 1, offset = 0, style = 'padding:0px;'),
    fluidRow(
      column(width = 12,
             fluidRow(style = "background-color: rgb(239,240,241);",
                      
                      avatar_box(src_img = "david.jpg", 
                                 name = "David Peel", 
                                 mail = "d.peel@lancaster.ac.uk",
                                 has_website = TRUE, 
                                 website = "http://www.lancaster.ac.uk/lums/people/david-peel"),
                      avatar_box(src_img = "william.jpg", 
                                 name = "William Tayler", 
                                 mail = "w.tayler@lancaster.ac.uk",
                                 has_website = TRUE, 
                                 website = "https://sites.google.com/site/williamjtayler/research"),
                      avatar_box(src_img = "alex.jpg", 
                                 name = "Alex Skouralis", 
                                 mail = "a.skouralis@lancaster.ac.uk",
                                 has_website = TRUE, 
                                 website = "http://www.research.lancs.ac.uk/portal/en/people/alex-skouralis(619bf93c-3841-4975-a9a9-e3f11a709287).html"),
                      avatar_box(src_img = "ben.jpg", 
                                 name = "Benjamin Finch", 
                                 mail = "b.finch@lancaster.ac.uk",
                                 has_website = FALSE)
             )
      )
    )
  )
)
    
