# an example to convert value to names
# Jan 10, 2012
#
# arrays defined
       facade   <- c("N00",  "S00",  "E00", "W00")
 names(facade)  <- c("north","south","east","west")
 
       SkyType  <- c("A2B",    "B2A",    "B3C",          "B4A",      "B5A",    "C5C")
 names(SkyType) <- c("Phoenix","Houston","San_Francisco","Baltimore","Chicago","Vancouver")

# a list of charater string
all.files.RAD <- list.files(path = Path.RAD,pattern = "\\.*\\.auto\\.photoCtrl8760\\.gz$",all.files=FALSE,full.names = FALSE, recursive = FALSE, ignore.case = FALSE)

# where the or.part has the code of facade and the st.part has the code of SkyType.
tmp      <- sub("\\.auto\\.photoCtrl8760\\.gz","",all.files.RAD)
tmp      <- unlist(strsplit(tmp,"_"))
or.part	 <- sub("or","",tmp[seq(from=1, to=length(tmp),by=15)])
st.part	 <- sub("st","",tmp[seq(from=2, to=length(tmp),by=15)])

# convert the oa.ppart and st.part from a list of the code to a list of the name
or.name <- names(facade)[match(or.part,facade)]
st.name <- names(SkyType)[match(st.part,SkyType)]
