# scripts/phylo_keys.R
# This script defines phylogenetic ordering of taxa encountered in deep sea data,
# at the phylum, class, order, and family levels.

# This can be updated and added to as additional taxa are encountered.


phylo_phylum <- c(
  "Foraminifera", "Microsporidia", "Porifera", "Cnidaria", 
  "Ctenophora", "Nemertea", "Annelida", "Mollusca", 
  "Arthropoda", "Echinodermata", "Chaetognatha", "Chordata"
)

phylo_class <- c(
  "Hexactinellida", "Hexacorallia", "Hydrozoa", "Octocorallia", "Scyphozoa",
  "Tentaculata", "Polychaeta", "Copepoda", "Malacostraca", "Cephalopoda",
  "Gastropoda", "Asteroidea", "Crinoidea", "Echinoidea", "Holothuroidea",
  "Ophiuroidea", "Appendicularia", "Myxini", "Holocephali", "Elasmobranchii", "Teleostei"
)

phylo_order <- c(
  "Actiniaria", "Anthoathecata", "Narcomedusae", "Trachymedusae", 
  "Siphonophora", "Siphonophorae", "Malacalcyonacea", "Scleralcyonacea", 
  "Coronatae", "Cydippida", "Terebellida", "Amphipoda", "Decapoda", 
  "Euphausiacea", "Mysida", "Nautilida", "Buccinida", "Paxillosida", 
  "Valvatida", "Comatulida", "Isocrinida", "Aspidodiadematoida", 
  "Cidaroida", "Diadematoida", "Echinothurioida", "Elasipodida", 
  "Euryalida", "Chimaeriformes", "Aulopiformes", "Lampriformes", 
  "Ophidiiformes", "Perciformes", "Siluriformes", "Stomiiformes", "Trachichthyiformes"
)

phylo_family <- c(
  "Stylasteridae", "Aeginidae", "Rhopalonematidae", "Acanthogorgiidae", 
  "Balticinidae", "Primnoidae", "Periphyllidae", "Acrocirridae", 
  "Eurytheneidae", "Acanthephyridae", "Aristeidae", "Calappidae", 
  "Crangonidae", "Eumunididae", "Geryonidae", "Homolidae", "Lithodidae", 
  "Lyreididae", "Munididae", "Munidopsidae", "Nematocarcinidae", 
  "Nephropidae", "Oregoniidae", "Pandalidae", "Parapaguridae", "Platymaia", 
  "Scyllaridae", "Sergestidae", "Solenoceridae", "Synaxidae", "Nautilus", 
  "Buccinidae", "Astropectinidae", "Goniasteridae", "Isselicrinidae", 
  "Aspidodiadematidae", "Cidaridae", "Diadematidae", "Echinothuriidae", 
  "Elpidiidae", "Euryalidae", "Rhinochimaeridae", "Chimaeridae", "Aulopidae", 
  "Lophotidae", "Ophidiidae", "Epigonidae", "Sternoptychidae", "Stomiidae", 
  "Trachichthyidae", "Halosauridae", "Macrouridae"
)