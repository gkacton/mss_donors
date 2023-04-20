# This script prepares the MSS_donors spreadsheet for use in this project.
# This script also prepares the Maine County Boundary shapefile for use with ggplot2.
# The Maine County Boundary shapefile can be downloaded from: 
  # https://maine.hub.arcgis.com/datasets/00033f9e376448fba27034f3d27d7ffb/explore

# load packages ---------------------------------------------------------------

library(dplyr)
library(sf)
library(ggplot2)
library(rmapshaper) 

# load MSS donor data ---------------------------------------------------------

mss_donors <- read.csv("MSS_donors.csv")


# making indicator column --> is donation from outside of Maine? ---------------

mss_donors$is_not_maine <- 0 # initializing the column

for(i in 1:nrow(mss_donors)){
  if(grepl(pattern = "nh", mss_donors$Location[i]) == TRUE){
    mss_donors$is_not_maine[i] <- 1
  }
  if(grepl(pattern = "n\\.h", mss_donors$Location[i]) == TRUE){
    mss_donors$is_not_maine[i] <- 1
  }
  if(grepl(pattern = "ma", mss_donors$Location[i]) == TRUE){
    mss_donors$is_not_maine[i] <- 1
  }
  # corrections --> towns that are in Maine but contain "nh" or "ma"
  if(mss_donors$Location[i] == "madison"|
     mss_donors$Location[i] == "burnham"|
     mss_donors$Location[i] == "bowdoinham"|
     mss_donors$Location[i] == "maine"|
     mss_donors$Location[i] == "madison") {
    mss_donors$is_not_maine[i] <- 0
  }
}

# removing all rows with NA in amount

mss_donors <- mss_donors[is.na(mss_donors$Amount) == FALSE,]

# separating Maine and non-Maine donations --------------------------------

mss_donors_notmaine <- mss_donors[mss_donors$is_not_maine == 1,]
mss_donors_maine <- mss_donors[mss_donors$is_not_maine == 0,]


# defining lists of towns in each county ----------------------------------
## including misspellings and alternate spellings found in the data

androscoggin <- c("auburn", "durham", "greene", "leeds", "lewiston", "lisbon", 
                  "livermore", "livermore falls", "mechanic falls", "minot", 
                  "poland", "sabattus", "turner", "wales")

aroostook <- c("caribou", "presque isle", "allagash", "amity", "ashland", 
               "bancroft", "blaine", "bridgewater", "castle hill", "caswell", 
               "chapman", "crystal", "dyer brook", "eagle lake", "easton", 
               "fort fairfield", "fort kent", "frenchville", "grand isle",
               "hamlin", "hammond", "haynesville", "hersey", "hodgdon", 
               "houlton", "island falls", "limestone", "linneus", "littleton", 
               "ludlow", "madawaska", "mapleton", "mars hill", "masardis",
               "merrill", "monticello", "new canada", "new limerick", 
               "new sweden", "oakfield", "orient", "perham", "portage lake",
               "saint agatha", "saint francis", "sherman", "smyrna", 
               "stockholm", "van buren", "wade", "washburn", "westfield",
               "westmanland", "weston", "woodland")

cumberland <- c("baldwin", "bridgton", "brunswick", "south brunswick", 
                "cape elizabeth", "casco", "chebeague island", "cumberland", 
                "falmouth", "freeport", "frye island", "gorham", "goham", 
                "gray", "harpswell", "south harpswell", "harrison", "harison", 
                "long island", "naples", "new gloucester", "north yarmouth", 
                "portland", "pownal", "raymond", "scarborough", "scarboro", 
                "sebago", "sabago", "south portland", "standish", "westbrook", 
                "windham", "yarmouth")

franklin <- c("avon", "carrabassett valley", "carthage", "chesterville", 
              "eustis", "farmington", "industry", "jay", "kingfield", "madrid", 
              "new sharon", "new vineyard", "phillips", "south phillips", 
              "rangeley", "strong", "temple", "weld", "wilton", "witon")

hancock <- c("ellsworth", "amherst", "aurora", "bar harbor", "blue hill", 
             "brooklin", "brooksville", "bucksport", "castine",
             "cranberry isles", "dedham", "deer isle", "eastbrook", "franklin", 
             "frenchboro", "gouldsboro", "great pond", "hancock", "lamoine", 
             "mariaville", "mount desert", "orland", "osborn", "otis", 
             "penobscot", "sedgwick", "sorrentoo", "southwest harbor",
             "stonington", "sullivan", "surry", "swans island", "tremont", 
             "trenton", "verona", "waltham", "winter harbor")


kennebec <- c("albion", "augusta", "belgrade", "benton", "chelsea", "china", 
              "clinton", "farmingdale", "fayette", "gardiner", "hallowell", 
              "lichfield", "manchester", "monmouth", "mount vernon", "oakland", 
              "pittston", "randolph", "readfield", "rome", "sidney", "sidney ", 
              "vassalboro", "vienna", "waterville", "waterville ", 
              "w. waterville", "wayne", "west gardiner", "windsor", "winslow", 
              "winthrop")

knox <- c("appleton", "camden", "camden ", "cushing", "friendship", "hope", 
          "isle au haut", "north haven", "owls head", "rockport", "st. george", 
          "south thomaston", "thomaston", "union", "vinalhaven", "warren", 
          "washington", "rockland")

lincoln <- c("alna", "boothbay", "boothbay ", "boothbay harbor", "bremen", 
             "bristol", "damariscotta", "dresden", "edgecomb", "edgeconch", 
             "jefferson", "newcastle", "nobleboro", "somerville", 
             "south bristol", "southport", "waldoboro", "westport", 
             "whitefield", "wiscasset", "monhegan")

oxford <- c("andover", "bethel", "brownfield", "buckfield", "byron", "canton",
            "denmark", "dixfield", "fryeburg", "gilead", "greenwood", "hanover", 
            "hartford", "hebron", "mt. hebron", "hiram", "lovell", "mexico", 
            "newry", "norway", "otisfield", "oxford", "paris", "peru", "porter",
            "roxbury", "rumford", "stoneham", "stowe", "sumner", "sweden",
            "upton", "waterford", "west paris", "woodstock")

penobscot <- c("bangor", "brewer", "old town", "alton", "bradford", 
               "burlington", "carmel", "charleston", "chester", "clifton",
               "corinna", "corinth", "east corinth", "dexter", "dixmont", 
               "dixmount", "east dixmont", "east dixmount", "e. dixmont", 
               "east millinocket", "eddington", "enfield", "etna", "exeter", 
               "garland", "glenburn", "greenbush", "greenfield", "hampden", 
               "hermon", "holden", "howland", "hudson", "indian island", 
               "kenduskeag", "lagrange", "lakeville", "lee", "levant", 
               "lincoln", "lowell", "mattawamkeag", "maxfield", "medway", 
               "milford", "millinocket", "mount chase", "newburgh", "newport",
               "orono", "orrington", "passadumkeag", "patten", "plymouth",
               "springfield", "stacyville", "stetson", "veazie", "winn", 
               "woodville")

piscataquis <- c("abbott", "atkinson", "south atkinson", "beaver cove", 
                 "bowerbank", "brownville", "dover-foxcroft", "greenville", 
                 "guilford", "medford", "milo", "monson", "parkman", 
                 "sangerville", "sebec", "shirley", "wellington", "willimantic")

sagadahoc <- c("bath", "arrowsic", "bowdoin", "bowdoinham", "georgetown", 
               "phippsburg", "richmond", "topsham", "west bath", "woolwich")

somerset <- c("anson", "athens", "bingham", "cambridge", "canaan", "cornville", 
              "detroit", "embden", "fairfield", "harmony", "hartland", 
              "jackman", "madison", "mercer", "moose river", "moscow", 
              "new portland", "norridgewock", "palmyra", "pittsfield", "ripley", 
              "saint albans", "skowhegan", "smithfield", "solon", "starks")

waldo <- c("belfast", "belmont", "brooks", "burnham", "frankfort", "islesboro", 
           "jackson", "knox", "liberty", "lincolnville", "monroe", "montville", 
           "morrill", "northport", "palermo", "prospect", "searsmont", 
           "searsport", "stockton springs", "swanville", "thorndike", "troy", 
           "unity", "waldo", "winterport")

washington <- c("calais", "eastport", "addison", "alexander", "baileyville", 
                "beals", "beddington", "centerville", "charlotte", 
                "cherryfield", "columbia", "columbia falls", "cooper", 
                "crawford", "cutler", "danforth", "deblois", "dennysville", 
                "east machias", "harrington", "indian township", "jonesboro", 
                "jonesport", "lubec", "machias", "machiasport", "marshfield",
                "meddybemps", "milbridge", "northfield", "pembroke", "perry", 
                "princeton", "robbinston", "roque bluffs", "steuben", 
                "talmadge", "topsfield", "vanceboro", "waite", "wesley", 
                "whiting", "whitneyville")

york <- c("biddeford", "bidderford", "saco", "acton", "alfred", "arundel", 
          "berwick", "buxton", "cornis", "dayton", "eliot", "hollis", 
          "kennebunk", "kennebunkport", "kittery", "lebanon", "limerick",
          "limington", "lyman", "newfield", "north berwick", "n. berwick", 
          "ogunquit", "old orchard beach", "parsonsfield", "sanford", 
          "shapleigh", "south berwick", "waterboro", "wells", "york")

# creating a vector of county names
maine_counties <- c("Androscoggin", "Aroostook", "Cumberland", "Franklin", "Hancock", 
                    "Kennebec", "Knox", "Lincoln", "Oxford", "Penobscot", "Piscataquis",
                    "Sagadahoc", "Somerset", "Waldo", "Washington", "York")


# assigning counties to each donation -------------------------------------

# initializing the new "county" column
mss_donors_maine$county <- " "
# removing rows with blanks in Location column
mss_donors_maine <- mss_donors_maine[mss_donors_maine$Location != "",]

for(i in 1:nrow(mss_donors_maine)){
  if(mss_donors_maine$Location[i] %in% androscoggin){
    mss_donors_maine$county[i] <- "Androscoggin"
  } else if(mss_donors_maine$Location[i] %in% aroostook){
    mss_donors_maine$county[i] <- "Aroostook"
  } else if(mss_donors_maine$Location[i] %in% cumberland){
    mss_donors_maine$county[i] <- "Cumberland"
  } else if(mss_donors_maine$Location[i] %in% franklin){
    mss_donors_maine$county[i] <- "Franklin"
  } else if(mss_donors_maine$Location[i] %in% hancock){
    mss_donors_maine$county[i] <- "Hancock"
  } else if(mss_donors_maine$Location[i] %in% kennebec){
    mss_donors_maine$county[i] <- "Kennebec"
  } else if(mss_donors_maine$Location[i] %in% knox){
    mss_donors_maine$county[i] <- "Knox"
  } else if(mss_donors_maine$Location[i] %in% lincoln){
    mss_donors_maine$county[i] <- "Lincoln"
  } else if(mss_donors_maine$Location[i] %in% oxford){
    mss_donors_maine$county[i] <- "Oxford"
  } else if(mss_donors_maine$Location[i] %in% penobscot){
    mss_donors_maine$county[i] <- "Penobscot"
  } else if(mss_donors_maine$Location[i] %in% piscataquis){
    mss_donors_maine$county[i] <- "Piscataquis"
  } else if(mss_donors_maine$Location[i] %in% sagadahoc){
    mss_donors_maine$county[i] <- "Sagadahoc"
  } else if(mss_donors_maine$Location[i] %in% somerset){
    mss_donors_maine$county[i] <- "Somerset"
  } else if(mss_donors_maine$Location[i] %in% waldo){
    mss_donors_maine$county[i] <- "Waldo"
  } else if(mss_donors_maine$Location[i] %in% washington){
    mss_donors_maine$county[i] <- "Washington"
  } else if(mss_donors_maine$Location[i] %in% york){
    mss_donors_maine$county[i] <- "York"
  }
}


# preparing map data ------------------------------------------------------

# loading in the county boundaries shapefile from Maine GeoLibrary

counties_shape <- read_sf("Maine_County_Boundary_Polygons_Feature")

# using rmapshaper::ms_simplify to reduce the resolution of the spatial data
  # helps the map run faster and take up less storage space

counties_simplified <- ms_simplify(counties_shape, 
                                   keep = 0.01, 
                                   keep_shapes = FALSE) # lets small shapes be
                                                          # removed 


# creating a new dataframe to calculate total donation + number of donations 
  # for each county

# initializing the new dataframe
county_totals <- as.data.frame(matrix(nrow = 16, ncol = 3, data = 0))
colnames(county_totals) <- c("county", "total", "num_donations")
county_totals$county <- maine_counties

# populating the dataframe 

for(i in 1:16){
  current_county <- maine_counties[i]
  for(j in 1:nrow(mss_donors_maine)){
    if(mss_donors_maine$county[j] == current_county){
      county_totals$total[i] <- county_totals$total[i] + mss_donors_maine$Amount[j]
      county_totals$num_donations[i] <- county_totals$num_donations[i] + 1
    }
  }
}

# merging the county totals in to the spatial dataframe
# will let us set link the "fill" aesthetic of map to total county donation

county_data_merged <- left_join(counties_simplified, county_totals, by = join_by("COUNTY" == "county"))

