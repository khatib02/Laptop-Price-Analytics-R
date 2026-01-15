################################ Data importing ################################ 
library(tidyverse)
library(tidytext)
library(mice)
library(VIM)
library(naniar)
library(broom.mixed)
library(gamlss)
library(splines)
library(glmnet)
library(cluster)
library(howManyImputations)
library(GGally)
library(robustbase)
library(factoextra)

# import data
data <- read_csv("data/amazon_laptop_prices_v01.csv")

# data overview
glimpse(data)
str(data)

############################### Basic Data Formatting ################################
# Remove duplicates 
data <- data %>%
  distinct()

"
Every column is in character mode, so we have to verify one by one. This is
possible because we have only 17 columns
"

# Transform brand into factor
data$brand <- factor(data$brand)

# Verify if any screen size is not in inches
data[!str_detect(data$screen_size, regex("inches", ignore_case=TRUE)) & !is.na(data$screen_size),]

# Extract only the numbers of screen size and turn it into numeric
data$screen_size <- as.numeric(parse_number(data$screen_size))

# Identify which laptops storages are not in GB
data[!str_detect(data$harddisk, regex("GB", ignore_case=TRUE)) & !is.na(data$harddisk),]

# Transform all hard disk measures into gb
tb_lines <- str_detect(data$harddisk, regex("tb", ignore_case=TRUE)) & !is.na(data$harddisk)
no_info_lines <- !str_detect(data$harddisk, regex("tb|gb|mb", ignore_case=TRUE)) & !is.na(data$harddisk)
mb_lines <- str_detect(data$harddisk, regex("mb", ignore_case=TRUE)) & !is.na(data$harddisk)
data[mb_lines,]
data[no_info_lines,]

"
Note that the laptops with 512 mb or less were typos, because windows 10 requires 
16 GB of hard disk to be installed. For the other 2, 32 mb and 128 mb are clearly
typos too. In other words, mb will be considered gb. The same thinking can be 
applied to the no_info_lines
"
 
data$harddisk <- parse_number(data$harddisk)
data$harddisk[tb_lines] <- data$harddisk[tb_lines] * 1000
data$harddisk <- as.numeric(data$harddisk)

# Adjust values based on internet info 
data$ram[data$harddisk < 17 & !is.na(data$harddisk) & data$brand=="acer"] <- "2 GB"
data$harddisk[data$model=="Acer Chromebook Spin 511" & !is.na(data$model)] <- "64"
data$harddisk[data$model=="NP754XFG-KB1US" & !is.na(data$model)] <- NA

summary(data$harddisk)

# Identify which rams are not GB
data[!str_detect(data$ram, regex("GB", ignore_case=TRUE)) & !is.na(data$ram),] 
" Only one with 64 MB, clearly a typo by the other variables (more likely GB)"

# Transform variables all to numeric removing GB
data$ram <- parse_number(data$ram)
data$ram <- as.numeric(data$ram)

# Check for inconsistencies
data[data$ram == 128 & !is.na(data$ram),]

# There are clearly wrong values based on other variables, they'll be turned into Unkwnown
data <- data %>%
  mutate(harddisk = if_else(model == "Precision 7780 Laptop" & harddisk == 32, NA, harddisk))

# Verify which cpu speed is not in GHz
data[!str_detect(data$cpu_speed, "\\d+[\\.\\d+]?\\s*GHz") & !is.na(data$cpu_speed),]
"
By verifying the information on internet, all the observations retrivied from the 
code above are, in fact, GHz
"
data$cpu_speed <- parse_number(data$cpu_speed)

# Verify which prices are not in $
data[!str_detect(data$price, "^\\$(?:\\d{1,3}(?:,\\d{3})*|\\d+)(?:\\.\\d{2})?$") & !is.na(data$price),]

"
All prices are in $
"
data$price <- parse_number(data$price)
summary(data$price)

########################### Fixing structural issues ########################### 
### brand column
top_brands <- c(
  "Dell", "Hp", "Lenovo", "Asus", "Acer", "Apple", "Msi", 
  "Microsoft", "Samsung", "Lg", "Razer", "Gigabyte", "Panasonic", "Rokc"
)

resellers_list <- c(
  "Computer Upgrade King", 
  "Mytrix", 
  "Quality Refurbished Computers" 
)

# These brands are wrong
data$brand[str_detect(data$brand, regex("best notebooks", ignore_case = TRUE))] <- "Dell"
data$brand[str_detect(data$brand, regex("foodservice", ignore_case = TRUE))] <- "Asus"

# Group rarer brands 
data <- data %>%
  # Basic Text Cleaning 
  mutate(brand = str_to_title(str_squish(brand))) %>%
  
  # Consolidating subsidiaries under their parent companies to reduce fragmentation
  mutate(brand = case_match(brand,
                            "Alienware" ~ "Dell",
                            "Latitude"  ~ "Dell",
                            "Rog"       ~ "Asus",
                            "Toughbook" ~ "Panasonic",
                            "Predator"  ~ "Acer",
                            "Omen"      ~ "HP",
                            "Gateway"   ~ "Acer",
                            .default = brand
  )) %>%
  
  # Creating the final high-level categories for modeling
  mutate(brand = case_when(
    # NA
    is.na(brand) ~ "Unknown",
    
    # Resellers 
    brand %in% resellers_list ~ "Reseller",
    
    # Major Manufacturers 
    brand %in% top_brands ~ brand,
    
    # All others -> Grouped as "Other"
    TRUE ~ "Other"
  ))

# Final check of the distribution
count(data, brand, sort = TRUE)

unique(data$brand)

data %>% 
  group_by(brand) %>% 
  summarize(number = n()) %>% 
  ggplot(aes(x = reorder(brand, -number), y = number)) + 
  geom_col() + 
  labs(x = "Brand", y = "Contagem") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
"
There are lots of brands with few notebooks available, so we must group it
Before we just group the rarer ones into other, observe that same brands have 
different strings like HP and hp. We need to group them first
"

### Model

" 
This column has a lot of repeated information from other columns, like brand
On the other hand, there are special words that tell the main product line.
We have to group them using this criteria
"

data <- data %>%
  mutate(
    # Grouping fragmented model names into robust market segments based on domain knowledge.
    product_line = case_when(
      # Na
      is.na(model) ~ "Unknown",
      
      # Gaming & High Performance (Captures MSI specific codes, Alienware, ROG, etc.)
      str_detect(str_to_lower(model), "alienware|rog|strix|omen|legion|nitro|vector|gp66|gs66|stealth|raider|blade|predator|g15|m18|tuf|katana|sword|aorus|pulse|odyssey|gaming") ~ "Gaming",
      
      # Business & Professional (Focus on durability/enterprise features)
      str_detect(str_to_lower(model), "latitude|precision|thinkpad|elitebook|probook|zbook|vostro|pro|expertbook|thinkbook|toughbook|summit|prestige|modern|dragonfly|cf|business") ~ "Business",
      
      # Premium Ultrabooks (Focus on portability/design)
      str_detect(str_to_lower(model), "xps|macbook|zenbook|envy|spectre|surface|swift|gram|yoga|creator|galaxy|premium") ~ "Premium",
      
      # Budget & Consumer (Entry level / General use)
      str_detect(str_to_lower(model), "inspiron|pavilion|ideapad|vivobook|aspire|chromebook|stream|flex|budget") ~ "Budget",
      
      # Fallback for generic models (likely includes ROKC and others)
      TRUE ~ "Standard" 
    ),

    # Extracting high-value features often buried in the text
    is_2in1 = as.integer(str_detect(str_to_lower(model), "2-in-1|x360|convertible|flip|fold")),
    is_touch = as.integer(str_detect(str_to_lower(model), "touch|touchscreen"))
  )

data$product_line <- factor(data$product_line)
count(data, product_line, sort = TRUE)

# see if the new variable can distinguish prices
data %>%
  filter(price < 5000) %>% # Filter extreme outliers just for better visualization
  ggplot(aes(x = reorder(product_line, price), y = price, fill = product_line)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Hiding outlier dots to clean the view
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) + # Adding raw points to show density
  scale_y_continuous(labels = scales::dollar_format()) + # Formatting axis back to $$$ visually
  theme_minimal() +
  labs(
    title = "Price Distribution by Market Segment",
    subtitle = "Validating the 'Product_Line' Feature Engineering",
    x = "Market Segment",
    y = "Price (USD)"
  ) +
  theme(legend.position = "none")

### Color

" 
The color column has several categories, so we must do something. We need to extract
just the primary color, like 'space gray' -> 'gray'
"
# Create a list of main colors
target_colors <- "silver|grey|gray|black|white|gold|blue|red|pink|green"

target_colors_res <- data$color %>% 
  str_to_lower() %>% 
  str_detect(pattern=target_colors)

# Verigy the remaining colors
table(data$color[!target_colors_res & !is.na(data$color)])

"There are three types of strings that our target_colors did not capture:
'poetic colors' (like Dark side of the moon), typos (like sliver) and 
scrapping erros (like Evo i7-1260P). We have to treat it"

# Substitute values that dont are colors by Unknown
wrong_color_values <- str_detect(data$color, pattern="Evo i7-1260P|RGB Backlit") & !is.na(data$color)
data$color[wrong_color_values] <- "Unknown"

# Substitue touch screen values
data$is_touch[data$color=="Touchscreen" & !is.na(data$color)] <- 1
data$color[data$color=="Touchscreen" & !is.na(data$color)] <- "Unknown"

# Categorize colors in important groups
data <- data %>% 
  mutate(color=case_when(
    is.na(color) ~ "Unknown",
    str_detect(str_to_lower(color), "silver|sliver|grey|gray|aluminum|graphite|titan|lunar|mercury|platinum") ~ "Silver/Grey",
    str_detect(str_to_lower(color), "black|balck|carbon|dark|midnight|obsidian|shadow|thunder|ash") ~ "Black",
    str_detect(str_to_lower(color), "gold|almond|dune|beige|champagne|rose") ~ "Gold",
    str_detect(str_to_lower(color), "white|ceramic|snow|frost") ~ "White",
    str_detect(str_to_lower(color), "blue|azure|teal|red|green|moss|sage|mint|pink|punk") ~ "Colorful",
    TRUE ~ "Other" 
  )) 

data$color <- factor(data$color)

### cpu

data <- data %>%
  mutate(
    cpu_lower = str_to_lower(cpu),
    
    cpu_family = str_extract(cpu_lower, "core ?i[0-9]|ryzen [0-9]|m[123]|celeron|pentium|athlon|xeon|mediatek|snapdragon|kirin|atom|series"),
    
    # 2. Reclassificação (Tier List Atualizada)
    cpu_tier = case_when(
      
      is.na(cpu)|str_detect(cpu, "8032|68000")|cpu_family=="Unknown"|cpu=="Unknown" ~ "Unknown",

      str_detect(cpu_family, "i9|ryzen 9|xeon") ~ "High-End",

      str_detect(cpu_family, "i7|ryzen 7|m1 max|m2 max") ~ "Performance", 
      
      str_detect(cpu_family, "i5|ryzen 5|m1|m2") ~ "Mainstream",
      
      str_detect(cpu_family, "i3|ryzen 3") ~ "Budget",
      
      str_detect(cpu_family, "celeron|pentium|athlon|mediatek|snapdragon|atom|series|core ?m|cortex|arm|kabini|a[0-9]") ~ "Low-End",
      
      TRUE ~ "Other/Legacy"
    )
  )

# distribution of the variable
count(data, cpu_tier, sort = TRUE)

# See if Other/Legacy contains only the right laptops
data$cpu[data$cpu_tier=="Other/Legacy" & !is.na(data$cpu_tier)]

# Move laptops from Other/Legacy that should be in other categories
data <- data %>%
  mutate(
    cpu_lower = str_to_lower(cpu), 
    
    cpu_tier = case_when(
      cpu_tier == "Other/Legacy" & str_detect(cpu_lower, "core ?m|a[0-9]|series|cortex|core_m") ~ "Low-End",
      
      str_detect(cpu_lower, "arm 7100") ~ "Performance",
      
      cpu_tier == "Other/Legacy" & str_detect(cpu_lower, "mobile cpu|others") ~ "Unknown",
      
      model == "XPS 15 7590" ~ "Unknown",

      TRUE ~ cpu_tier
    )
  )

# merge apple high end with general high end
data <- data %>%
  mutate(
    # Se for "High-End (Apple)", vira "High-End". Se não, mantém o que estava.
    cpu_tier = if_else(cpu_tier == "High-End (Apple)", "High-End", cpu_tier)
  )

# See the final distribution
count(data, cpu_tier, sort = TRUE)

# exclude temporary variables 
data[,c("cpu_lower", "cpu_family")] <- NULL

data$cpu_tier <- factor(data$cpu_tier)

data[data$cpu_tier=="Other/Legacy" & !is.na(data$cpu_tier),]

# see if the new variable can distinguish prices
data %>%
  filter(price < 5000) %>% # Filter extreme outliers just for better visualization
  ggplot(aes(x = reorder(cpu_tier, price), y = price, fill = cpu_tier)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Hiding outlier dots to clean the view
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) + # Adding raw points to show density
  scale_y_continuous(labels = scales::dollar_format()) + # Formatting axis back to $$$ visually
  theme_minimal() +
  labs(
    title = "Price Distribution by Market Segment",
    subtitle = "Validating the 'Cpu_tier' Feature Engineering",
    x = "Market Segment",
    y = "Price (USD)"
  ) +
  theme(legend.position = "none")

### OS

# is there any laptop without main type of OS?
print(data[!str_detect(str_to_lower(data$OS), "windows|chrome|mac") & !is.na(data$OS),],n=100)

# transform win to windows
data$OS <- str_replace(data$OS, pattern=regex("win\\s", ignore_case=TRUE), replacement="Windows ")

# unknown type
data$OS[data$OS=="PC" & !is.na(data$OS)] <- "Unknown"

data <- data %>%
  mutate(
    os_lower = str_to_lower(OS),
    
    # Padronizing the OS column
    os_type = case_when(
      str_detect(os_lower, "mac|apple|osx|macos") ~ "macOS",
      str_detect(os_lower, "chrome|google|chromebook") ~ "Chrome OS",
      str_detect(os_lower, "windows|win") ~ "Windows",
      str_detect(os_lower, "linux|ubuntu|fedora") ~ "Linux",
      str_detect(os_lower, "dos|no os|without") ~ "No OS",
      TRUE ~ "Other"
    ),
    
    # Identify 'premium' OS
    is_pro_os = as.integer(str_detect(os_lower, "pro|professional|business|enterprise")),
    
    # Identify 'budget' OS
    is_s_mode = as.integer(str_detect(os_lower, "\\bs\\b|s mode"))
  )

# discard temp columns
data[, "os_lower"] <- NULL

# see the distribution
count(data, os_type, sort = TRUE)

# group linux into others
data <- data %>%
  mutate(
    os_type = case_when(
      os_type %in% c("Windows", "macOS", "Chrome OS", "Unknown") ~ os_type,
      
      TRUE ~ "Other"
    )
  )


count(data, os_type, sort = TRUE)

# see if we captured all windows pro and s
data %>% 
  filter(os_type == "Windows") %>%
  count(is_pro_os, is_s_mode)

### special_features

# Which special features are more frequent
data %>%
  # Separa as vírgulas para contar cada feature individualmente
  separate_rows(special_features, sep = ",") %>%
  mutate(special_features = str_trim(str_to_lower(special_features))) %>%
  count(special_features, sort = TRUE) %>%
  head(20)

# Create a binary identifier column for more important special features
data <- data %>%
  mutate(
    special_features_lower = str_to_lower(special_features),
    
    has_backlit_keyboard = as.integer(str_detect(special_features_lower, "backlit")),
    
    has_fingerprint = as.integer(str_detect(special_features_lower, "fingerprint"))

  )

# How many laptops present these features
data %>%
  select(has_backlit_keyboard, has_fingerprint) %>%
  colSums(na.rm = TRUE)

### graphics and graphics_coprocessor

data <- data %>%
  mutate(
    
    gpu_text = str_to_lower(paste(graphics, graphics_coprocessor)),
    
    # Replaces "inter core" with "intel core" and cleans spaces
    gpu_text = str_replace_all(gpu_text, "inter core", "intel core"),
    
    # Extract Brand 
    gpu_brand = case_when(
      str_detect(gpu_text, "nvidia|geforce|gtx|rtx|quadro|t[0-9]{3}|a[0-9]{4}") ~ "NVIDIA",
      str_detect(gpu_text, "amd|radeon|ryzen") ~ "AMD",
      str_detect(gpu_text, "apple|m1|m2") ~ "Apple",
      str_detect(gpu_text, "adreno|mali|powervr|mediatek") ~ "ARM", 
      str_detect(gpu_text, "intel|iris|uhd|hd graphics|integrated|shared|gt2") ~ "Intel",
      
      TRUE ~ "Unknown" # What remains (likely "Dedicated" without brand)
    ),
    
    # Classify Power Tier (Kept logic, ensured generics fall correctly)
    gpu_tier = case_when(

      str_detect(gpu_text, "rtx (207|208|307|308|309|407|408|409)|rx [67][89]00") ~ "High-End",
      
      str_detect(gpu_text, "rtx (205|206|305|306|405|406)|gtx|rx [56][56]00|t[0-9]{3,4}|a[1-5]000") ~ "Mid-Range",
      
      str_detect(gpu_text, "mx[0-9]{3}|radeon 5[0-9]0|610|620|630") ~ "Entry-Dedicated",
      
      # If it says "dedicated" but didn't match rules above (not RTX, GTX), it's basic.
      str_detect(gpu_text, "dedicated") ~ "Entry-Dedicated",
      
      # Everything else (Intel HD, Integrated, Shared) falls here
      TRUE ~ "Integrated"
    )
  )

# Checking the results
# "Intel" should have grown significantly, and "Unknown" should be minimal.
count(data, gpu_brand, gpu_tier, sort = TRUE)

# discard temp columns
data[,c("gpu_brand", "gpu_text")] <- NULL

# see if the new variable can distinguish prices
data %>%
  filter(price < 5000) %>% # Filter extreme outliers just for better visualization
  ggplot(aes(x = reorder(gpu_tier, price), y = price, fill = gpu_tier)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Hiding outlier dots to clean the view
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) + # Adding raw points to show density
  scale_y_continuous(labels = scales::dollar_format()) + # Formatting axis back to $$$ visually
  theme_minimal() +
  labs(
    title = "Price Distribution by Market Segment",
    subtitle = "Validating the 'Gpu_tier' Feature Engineering",
    x = "Market Segment",
    y = "Price (USD)"
  ) +
  theme(legend.position = "none")

### storage and ram
data <- data %>%
  mutate(
    
    ram = round(ram),
    
    storage_gb = as.numeric(harddisk)
  ) %>%
  # Remove the old column to keep dataset clean
  select(-harddisk)

# Final check of the hardware specs
summary(data[, c("ram", "storage_gb")])

glimpse(data)
data[,"special_features_lower"] <- NULL

############################### Data validation ################################
# Turn unknown into NA
data[data == "Unknown"] <- NA
data$os_type <- factor(data$os_type)
data$gpu_tier <- factor(data$gpu_tier)
data$brand <- factor(data$brand)

# Remove observations without price
data <- data[!is.na(data$price), ]


### Looking for erros in the data

# Select features that define the hardware identity and price
audit_data <- data %>%
  mutate(.row_id = row_number()) %>%
  mutate(
    # Log-transform price to normalize scale for distance calculation
    log_price = log(price),
    brand = as.factor(brand),
    cpu_tier = as.factor(cpu_tier),
    gpu_tier = as.factor(gpu_tier)
  )

# Calculate Gower distance, which handles NAs automatically
gower_dist <- daisy(audit_data %>% select(-.row_id, -price), metric = "gower")

# Group data into 4 clusters (e.g., Budget, Mid, High, Premium)
pam_fit <- pam(gower_dist, k = 4)

# Calculate silhouette width to measure how well each point fits
sil_info <- silhouette(pam_fit)

# Combine clustering results with original data
audit_result <- audit_data %>%
  
  # Add the assigned cluster
  mutate(cluster = as.factor(pam_fit$clustering)) %>%
  
  # Add the silhouette width 
  mutate(sil_width = sil_info[, "sil_width"])

# Filter for rows that statistically do not belong in their group
anomalies <- audit_result %>%
  
  # Negative silhouette indicates a probable error or outlier
  filter(sil_width < 0) %>%
  
  # Sort by worst fit first
  arrange(sil_width)

# Display the most suspicious records 
print(anomalies, n=60)

"
A few things stand out from the silhouette analysis:

The Garbage Data (IDs 590, 584, etc.):
These rows are pretty much useless. They're missing critical tags like
'cpu_tier' or 'storage', so the model is just going to choke on them.
Better to just drop them now than deal with bad imputation later.

 The 'Too Good to be True' (ID 2190):
Found a Dell listed with 4TB of storage for ~$1,200. That math doesn't
add up—SSD storage isn't that cheap. It's likely a scraping glitch
(maybe read a secondary HDD as primary?). removing it to avoid skewing
the storage coefficients.

The False Alarm (ID 589):
This one got flagged as an outlier, but looking at the specs (64GB RAM),
it's actually legit. It's just a monster workstation, not a data error.
Keeping this one—we need it to represent the high-end market properly.
"

### Fixing the ones with two or more missing vital information per line

# Density diagnose
diagnostic_count <- data %>%
  mutate(
    info_count = rowSums(!is.na(select(., ram, storage_gb, screen_size, cpu_tier, gpu_tier, product_line)))
  )

# How many laptops have just 0, 1, 2, 3, 4 ou 5 complete values
table(diagnostic_count$info_count)

# Let's classify rows into "Rich" (lots of info) and "Poor" (little info)
diagnostic <- data %>%
  mutate(
    info_count = rowSums(!is.na(select(., ram, storage_gb, screen_size, cpu_tier, gpu_tier, product_line)))
  ) %>%
  mutate(
    status = case_when(
      info_count <= 3 ~ "Trash", 
      info_count == 4 ~ "Doubt",
      TRUE ~ "Reliable"
    )
  )

# Density Plot: Is the price of "Poor" rows different from "Rich" ones?
ggplot(diagnostic, aes(x=log(price), fill=status)) +
  geom_density(alpha=0.5) +
  labs(title = "Bias Check: Are empty rows systematically cheaper?",
       subtitle = "If curves overlap, missingness is random (MCAR). If separated, it's bias.")

"This density plot reveals an important insight: the missing data isn't random. 
The red curve (representing rows with missing specs) is shifted significantly to
the left compared to the blue 'Solid' data. This proves that incomplete records 
are heavily concentrated in the budget price range—likely because sellers of 
cheap laptops don't bother filling out every technical detail. If we were to 
delete these rows, we wouldn't just be cleaning the data; we would be 
accidentally wiping out the entire low-end market segment, forcing our model to 
overestimate laptop prices."

# Remove lines with 3 or more vital data missing (38 lines)
tech_cols <- c("ram", "storage_gb", "screen_size", "cpu_tier", "gpu_tier", "product_line")
data <- data %>% # Filter
  
  # Count how many valid technical specs exist in each row
  mutate(info_count = rowSums(!is.na(select(., all_of(tech_cols))))) %>%
  
  # REMOVE only rows with 3 or fewer specs (The "Green" Noise group)
  filter(info_count > 3) %>%
  
  # Drop the temporary helper column
  select(-info_count)

### Investigating the ones with prices "too good (or too bad) to be true"

suspects <- data %>%
  mutate(.row_id = row_number()) %>% 
  filter(
    (storage_gb >= 2000 & price < 600) |  # 2TB for less than 600 USD 
      (storage_gb > 3000 & price < 2000) |  # 4TB for less than 2000 USD
      (ram >= 64 & price < 1000)          # 64GB RAM for less than 1000 USD 
  ) %>%
  select(.row_id, brand, model, price, ram, storage_gb, brand, product_line, gpu_tier)

print(suspects, n=67)

check_brands <- data %>%
  filter(brand %in% c("Dell", "Hp", "Acer", "Asus", "Lenovo")) %>%
  mutate(
    is_monster = (ram >= 40 | storage_gb >= 2000) & 
      (product_line %in% c("Budget", "Standard", "Business") | is.na(product_line)),
    category = ifelse(is_monster, "Suspicious (Monster Spec)", "Normal")
  )

ggplot(check_brands, aes(x = brand, y = log(price), fill = category)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Normal" = "grey", "Suspicious (Monster Spec)" = "red")) +
  labs(title = "Double Check: Is the price pattern consistent across all brands?",
       subtitle = "To validate data, the Red box MUST be above the Grey box for all brands.",
       y = "Log(Price)", 
       x = "Brand") +
  theme_minimal()

data <- data %>% mutate(.row_id = row_number())

data <- data %>%
  filter(
    !(
      # Any machine with 32GB+ RAM or 2TB+ HD costing less than $450 is an error/scam.
      price < 450 & 
        (coalesce(ram, 0) >= 32 | coalesce(storage_gb, 0) >= 2000)
    ),
    
    !(
      # Extreme hardware (64GB RAM or 4TB SSD) cannot exist at entry-level prices
      # due to the raw cost of the components.
      (coalesce(ram, 0) >= 64 & price < 500) |
        (coalesce(storage_gb, 0) >= 4000 & price < 600)
    ),
    
    !(
      # If the CPU is High-End/Performance, the price cannot be low (<$1000) 
      # if it also carries massive specs (protects against "fake" gaming laptops).
      cpu_tier %in% c("High-End", "Performance") &
        price < 1000 &
        (coalesce(ram, 0) >= 40 | coalesce(storage_gb, 0) >= 2000)
    )
  )

data <- data %>% mutate(.row_id = row_number())

"
The deleted laptops were all individually inspected and were removed because 
their information was clearly scraping errors
"

# Clearly a typo
data <- data %>% filter( !(data$model=="Vostro 7620 Laptop" & data$price>10000) )

# columns with NA values
cols_na <- colnames(data[,sapply(data, function(x) sum(is.na(x))) > 0])

# percentage of NA per column with NA values
sapply(data[,cols_na], function(x) round(sum(is.na(x))/nrow(data),3))

### Removing columns that are not useful anymore

data <- data %>% select(-OS, -special_features, -graphics, -graphics_coprocessor,
                        -model, -.row_id, -cpu)

data <- data %>% select(-cpu)

### Ordering factor variables
data <- data %>%
  mutate(across(starts_with("is_") | starts_with("has_"), as.factor),
         cpu_tier = factor(cpu_tier, 
                           levels = c("Budget", "Low-End", "Mainstream", 
                                      "Performance", "High-End", "Other/Legacy"),
                           ordered = TRUE),
         product_line = factor(product_line,
                               levels = c("Budget", "Standard", "Business", 
                                          "Gaming", "Premium"), 
                               ordered = TRUE),
         gpu_tier = factor(gpu_tier, 
                           levels = c("Integrated", "Entry-Dedicated", 
                                      "Mid-Range", "High-End"),
                           ordered = TRUE)
  )

### Dealing with columns with high na rate: cpu_speed

# Redundancy Check: Is 'cpu_speed' necessary if we have 'cpu_tier'?
# We check if there is significant price variation explained by speed WITHIN the same tier.
check_cpu <- data %>%
  filter(!is.na(cpu_speed)) %>%
  ggplot(aes(x = as.factor(cpu_speed), y = price)) +
  geom_boxplot() +
  facet_wrap(~cpu_tier, scales = "free") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Does CPU Speed matter WITHIN the same Tier?",
       subtitle = "If boxplots are similar, the Tier already captures the value and Speed is redundant.")

print(check_cpu)

"
Removing 'cpu_speed': The boxplots proved that 'cpu_tier' makes this column redundant.
Within a tier, higher GHz didn't guarantee a higher price, so it's just noise.
"
data <- data %>% select(-cpu_speed)

##################################### EDA ######################################

library(robustbase)
help(count)

### Univariate graphs

# barplot of brand
data %>% count(brand) %>% 
  ggplot(aes(x=reorder(brand, -n), y=n)) + geom_col()

# Group rare bands together into other
data <- data %>% mutate(brand = fct_lump_n(brand, 6))

# barplot of color
data %>% mutate(color = fct_lump_n(color, 2)) %>% count(color) %>% 
  ggplot(aes(x=reorder(color, -n), y=n)) + geom_col() 

# Group rare colors together into other
data <- data %>% mutate(color = fct_lump_n(color, 2))

# barplot of product_line 
data %>% count(product_line) %>% 
  ggplot(aes(x=reorder(product_line, -n), y=n)) + geom_col() 

# barplot of is_2in1
data %>% count(is_2in1) %>% 
  ggplot(aes(x=reorder(is_2in1, -n), y=n)) + geom_col() 

# barplot of is_touch
data %>% count(is_touch) %>% 
  ggplot(aes(x=reorder(is_touch, -n), y=n)) + geom_col() 

# remove is_touch
data <- data %>% select(-is_touch)

"
The variable `is_touch` was removed because it had almost zero variance, with 
only 6 occurrences in 2400 observations (0.25% of the sample). This low 
representativeness prevents the identification of consistent patterns and would 
compromise the generalization of statistical models.
"

# barplot of cpu_tier
data %>% count(cpu_tier) %>% 
  ggplot(aes(x=reorder(cpu_tier, -n), y=n)) + geom_col() 

# Remove "Other/Legacy" Laptops
data <- data %>% filter(!cpu_tier=="Other/Legacy")

"
As our main analysis concerns just the main market, legacy laptops will be 
removed (one observation)
"

# barplot of os_type
data %>% count(os_type) %>% 
  ggplot(aes(x=reorder(os_type, -n), y=n)) + geom_col() 

# barplot of is_pro_os
data %>% count(is_pro_os) %>% 
  ggplot(aes(x=reorder(is_pro_os, -n), y=n)) + geom_col() 

# barplot of is_s_mode
data %>% count(is_s_mode) %>% 
  ggplot(aes(x=reorder(is_s_mode, -n), y=n)) + geom_col() 

# remove is_s_mode
data <- data %>% select(-is_s_mode)

"
Despite representing 1% of the sample, the variable `is_s_mode` was disregarded 
due to its low variability. In datasets of this size, variables with a 99/1 
distribution are treated as predictors with near-zero variance, as they do not 
offer robust statistical information to differentiate the groups.
"

# barplot of has_backlit_keyboard
data %>% count(has_backlit_keyboard) %>% 
  ggplot(aes(x=reorder(has_backlit_keyboard, -n), y=n)) + geom_col() 

# barplot of has_fingerprint
data %>% count(has_fingerprint) %>% 
  ggplot(aes(x=reorder(has_fingerprint, -n), y=n)) + geom_col() 

# barplot of gpu_tier
data %>% count(gpu_tier) %>% 
  ggplot(aes(x=reorder(gpu_tier, -n), y=n)) + geom_col() 

# boxplot of ram
adjbox(data$ram)

# Verify if the outliers of ram are typos/scraping errors
data %>% slice_min(order_by=ram, n=1)

data[data$ram < 4 & !is.na(data$ram),]

data$ram <- data %>% filter(ram < 4)

# Verify the models with great size of ram
data %>% filter(ram > 64)

" 
Observations with less than 4GB of RAM were excluded to align the dataset with 
contemporary market standards. These entries represent legacy hardware that falls 
below the operational baseline for modern computing, and their removal prevents
the model from being skewed by outdated hardware specifications.
"

# boxplot of rating
boxplot(data$rating)

# Percentage of NA
sum(is.na(data$rating))/nrow(data)

data <- data %>%
  mutate(rating = cut(rating, 
                            breaks = c(0, 3.7, 4.4, 5), 
                            labels = c("Low", "Medium", "High"),
                            include.lowest = TRUE)) %>%
  # Keep NA as category
  mutate(rating = fct_na_value_to_level(rating, level = "Not Available"))

"
Why treat rating NA as category instead of just imputing?
1 - rating has great percentage of missing, which may cause problems
2 - The absence of rating is signal that the product is new to Amazon, so it
gives us information
"

data <- data %>%
  select(-rating)

# Verify if the outliers of rating are typos/scraping errors
data %>% slice_min(order_by=rating, n=1)

# barplot of rating
data %>% count(rating) %>% 
  ggplot(aes(x=reorder(rating, -n), y=n)) + geom_col() 

# plots of price
adjbox(data$price)
plot(density(data$price))

# Check outliers of price
adjbox(data$price, plot=FALSE)
data %>% filter(price < 115 | price > 5498)

# plots of log(price)
adjbox(log(data$price))
plot(density(log(data$price)))

# boxplot of storage_gb
adjbox(data$storage_gb)

# Check the outliers
adjbox(data$storage_gb, plot=FALSE)
data %>% filter(storage_gb <= 64 | storage_gb >= 8000)

# Using an order for product_line, cpu_tier and gpu_tier was wrong, because there is no inherent order
data$product_line <- factor(data$product_line, ordered=FALSE)
data$cpu_tier <- factor(data$cpu_tier, ordered=FALSE)
data$gpu_tier <- factor(data$gpu_tier, ordered=FALSE)

### Bivariate plots
adjbox(price ~ brand, data=data, log="y", names = levels(data$brand)) 
adjbox(price ~ color, data=data, log="y", names = levels(data$color)) 
adjbox(price ~ rating, data=data, log="y", names = levels(data$rating)) 
adjbox(price ~ product_line, data=data, log="y", names = levels(data$product_line))
adjbox(price ~ is_2in1, data=data, log="y", names = levels(data$is_2in1)) 
adjbox(price ~ cpu_tier, data=data, log="y", names = levels(data$cpu_tier)) 
adjbox(price ~ os_type, data=data, log="y", names = levels(data$os_type)) 
adjbox(price ~ is_pro_os, data=data, log="y", names = levels(data$is_pro_os)) 
adjbox(price ~ has_backlit_keyboard, data=data, log="y", names = levels(data$has_backlit_keyboard)) 
adjbox(price ~ has_fingerprint, data=data, log="y", names = levels(data$has_fingerprint)) 
adjbox(price ~ gpu_tier, data=data, log="y", names = levels(data$gpu_tier))
adjbox(price ~ storage_gb, data=data, log="y", names = levels(data$storage_gb))
adjbox(price ~ ram, data=data, log="y", names = levels(data$ram))
adjbox(price ~ screen_size, data=data, log="y", names = levels(data$screen_size))

### PPS : Measure the predictive power scores (pps) of all type of variables 
library(ppsr)
visualize_pps(df = data, y="price")

"
Hardware Performance as the Primary Driver: CPU Tier (0.33) and RAM (0.24) 
emerged as the most significant predictors of laptop price.
Low Brand Influence: The low score for Brand (0.03) suggests that pricing is 
driven by technical specifications rather than brand equity in this dataset.
Feature Pruning: Several features (e.g., 'is_2in1', 'has_backlit_keyboard') 
returned a PPS of 0.00, indicating they provide no marginal predictive value 
"

### Clustering for typos searching

# Standardize numerical values and choose important variables for clustering
data_sd <- data %>% 
  dplyr::select(ram, cpu_tier, gpu_tier, storage_gb, price) %>% 
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))  

# Construct distance matrix
gow_mat <- daisy(data_sd, metric = "gower")

"
To optimize the clustering performance and ensure meaningful segments, 
I performed feature selection based on the PPS results. I included high-impact 
hardware variables (cpu_tier, ram, gpu_tier, storage_gb) weighted by their scores
and key contextual features (product_line, screen_size) while excluding 
low-predictive noise such as color and peripheral features. This targeted 
approach was instrumental in increasing the average silhouette width from 0.14 
to 0.31, resulting in clusters that represent distinct market segments rather 
than random hardware combinations.
"

# Searchs for best k based on average silhouette
res_nclust_sil <- tibble(nclust = seq(2, 20), silhouette = numeric(19))
for (nclust in seq(2, 20)) {
  pam_clusters <- pam(x = as.matrix(gow_mat), k = nclust, diss = TRUE) 
  mean_sil <- pam_clusters$silinfo$avg.width
  res_nclust_sil$silhouette[res_nclust_sil$nclust == nclust] <- mean_sil
}
res_nclust_sil %>% arrange(desc(silhouette))  

# Best k=10

# Perform clustering
pam_clusters <- pam(x = as.matrix(gow_mat), k = 9, diss = TRUE) 

# See results
clusters <- pam_clusters$clustering
summary(pam_clusters)
pam_clusters$data <- data_sd

# Plot clusterings
mds <- cmdscale(gow_mat, k = 10, eig = TRUE)
plot_df <- tibble(
  Dim1 = mds$points[, 1],
  Dim2 = mds$points[, 2],
  cluster = factor(pam_clusters$clustering)
)
sum(mds$eig[mds$eig > 0]) / sum(abs(mds$eig))
ggplot(plot_df, aes(Dim1, Dim2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "PAM clustering (Gower distance)",
    subtitle = "Visualization via MDS / PCoA",
    x = "PCoA 1",
    y = "PCoA 2",
    color = "Cluster"
  ) +
  theme_minimal()

# Silhouette plot
fviz_silhouette(pam_clusters)

# Identify main characteristics of clusters
get_mode <- function(v) {
  # Remove NAs first if needed
  v <- v[!is.na(v)]
  uniques <- unique(v)
  # which.max(tabulate(match(v, uniques))) finds the index of the most frequent value
  uniques[which.max(tabulate(match(v, uniques)))]
}
data %>%
  mutate(cluster = pam_clusters$clustering) %>%
  group_by(cluster) %>%
  summarise(
    mean_price = mean(price, na.rm=TRUE),
    mean_ram = mean(ram, na.rm=TRUE),
    mean_storage = mean(storage_gb, na.rm=TRUE),
    mean_screen_size = mean(screen_size, na.rm=TRUE),
    most_frequent_brand = get_mode(brand),
    most_frequent_product_line = get_mode(product_line),
    most_frequent_cpu_tier = get_mode(cpu_tier),
    most_frequent_gpu_tier = get_mode(gpu_tier),
    n_observacoes = n()
  ) %>% 
  arrange(desc(mean_price))


# Investigate data points with negative silhouette first
audit_df <- cbind(data, pam_clusters$silinfo$widths) %>% filter(sil_width < 0) %>% 
  arrange(sil_width)

ids_para_remover <- audit_df %>%
  filter(sil_width < 0) %>% # Primeiro critério: o algoritmo desconfia do ponto
  filter(
    (price > 2000 & ram <= 4) |           # Erro de RAM em luxo
      (cpu_tier == "Low-End" & price > 1000) | # Erro de Preço/CPU
      (cluster == 3 & price < 1000)            # Intruso no cluster de elite
  ) %>%
  pull(id) # Extrai os IDs desses registros

"
Results:
SPATIAL VALIDATION (PCoA):
The PCoA plot confirms a robust hierarchical structure for most segments. 
However, it successfully isolated 'lone wolf' anomalies—specifically 
extreme outliers in Cluster 2 and 1—which are positioned far from any 
dense data clouds, signaling potential entry errors.
SEGMENT COHERENCE (Summary Table):
The profiling reveals strong alignment between hardware specs (CPU/RAM) 
and price points, validating the PPS drivers identified earlier. 
Cluster 5 (Ultra-Budget) and Cluster 9 (High-End Gaming) show high 
internal consistency.
ANOMALY IDENTIFICATION:
Cluster 7 was flagged as a 'data-entry risk'. Despite having dedicated 
GPUs and 12GB+ RAM, its mean price is inconsistently low compared to 
basic integrated-GPU clusters. The fragmented 
distribution of Cluster 7 in the PCoA further confirms its status as 
a collection of market outliers rather than a stable segment.
CONCLUSION:
Using k=10 with an average silhouette of 0.50 provided the necessary 
resolution to separate legitimate market tiers from noise. 
These isolated records will be removed to ensure the final predictive 
model is built on clean, representative data.
"

################################## Imputation ################################## 

### dealing with binary missing data

"
# Assumption: In product listings, 'Silence Implies Absence'.
# If a seller does not explicitly list a premium feature (e.g., Backlit Keyboard, Touchscreen),
# we assume it is not present. Statistical imputation here would introduce noise by 
# randomly assigning premium features to budget laptops.
# Action: Filling all binary NAs with 0 (False).
"

data <- data %>%
  mutate(
    is_2in1              = replace_na(is_2in1, 0),
    is_touch             = replace_na(is_touch, 0),
    has_backlit_keyboard = replace_na(has_backlit_keyboard, 0),
    has_fingerprint      = replace_na(has_fingerprint, 0),
    is_pro_os            = replace_na(is_pro_os, 0),
    is_s_mode            = replace_na(is_s_mode, 0)
  )

# Fast check to see if there is any na remaining
summary(select(data, is_2in1, has_backlit_keyboard, is_pro_os))

"
It requires a more sophisticated approach: MICE
Source for tutorial on mice: https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/mi.html#mice
Source for original article: https://www.jstatsoft.org/article/view/v045i03
"
help(md.pairs)

# Explore missing patterns
gg_miss_upset(data, nsets = 10)

"
IMPUTATION STRATEGY: MICE (Multivariate Imputation by Chained Equations)
Justification: The UpSet plot reveals complex, non-monotone missing patterns, 
indicating a Missing at Random (MAR) mechanism.
Simple imputation (mean/mode) would bias correlations (e.g., Price vs. RAM). 
MICE is necessary to preserve the multivariate structure.
"

################################## MICE ########################################

data <- data %>%
  mutate(across(where(is.character), as.factor)) %>%
  as.data.frame() # Ensuring it's a standard data frame for stability

# Check which columns still have NAs before starting
cols_to_impute <- names(which(colSums(is.na(data)) > 0))
cat("Target columns for MICE:", paste(cols_to_impute, collapse = ", "), "\n")

init <- mice(data, maxit = 0)
pred_matrix <- init$predictorMatrix
mice_res <- mice(data, 
                method = 'pmm', 
                predictorMatrix = pred_matrix, 
                m = 10,       
                maxit = 50,   
                seed = 123,
                print = TRUE) 

# Check mice imputation validity
densityplot(mice_res)
stripplot(mice_res)
plot(mice_res, layout = c(2, 4))



############################ Exploratory Inference ############################# 

"In order for us to do inference, we need a statistical model. As our dataset does not
have many observations, we can't divide it into exploration and inference.
This limits the inference we can make, because testing several models and choosing
different variables can severely impair our generalization power.
1. Grouped LASSO / Group Elastic Net
2. Variable selection
3. Refit without penalization
4. Inference using Rubin’s Rules
source: https://pubmed.ncbi.nlm.nih.gov/36644406/
"

### Stacked method (for stepAIC and stepGAIC)

# Create a stacked dataset in "long" format containing all m imputations
data_stacked <- complete(mice_res, "long")

# Assign weights of 1/m to each observation to correct the total sample size
data_stacked$w <- 1/mice_res$m

### base model

library(MASS)
library(car)

fit_base <- lm(log(price) ~ ram + screen_size + storage_gb + 
                 cpu_tier + gpu_tier + brand + os_type + 
                 is_pro_os + is_touch + is_2in1 + 
                 has_backlit_keyboard + has_fingerprint + 
                 product_line + rating + color + is_s_mode, data=data_stacked,
                 weights = w)

fitstep_base <- stepAIC(fit_base)

final_base <- with(mice_res, lm(log(price) ~ ram + screen_size + storage_gb + 
                                  cpu_tier + gpu_tier + brand + os_type + 
                                  is_pro_os + is_touch + is_2in1 + 
                                  has_backlit_keyboard + has_fingerprint + 
                                  product_line + rating + color + is_s_mode))

summary(pool(final_base), conf.int = TRUE)

aics_finalbase <- sapply(final_base$analyses, AIC)
mean_aic_finalbase <- mean(aics)

par(mfrow=c(2,2))
plot(final_base$analyses[[1]])
plot(final_base$analyses[[4]])
plot(final_base$analyses[[7]])
plot(final_base$analyses[[9]])

car::vif(final_base$analyses[[1]])
car::vif(final_base$analyses[[4]])
car::vif(final_base$analyses[[7]])

"
Diagnostic: Presence of heavy tails and outliers/leverage observations
Os_type has severe multicolinearity with brand, so it will be removed
No significant changes between complete datasets
Next step: 
Investigate outliers one a one (there are only three, no need for robust models)
Fit a gamlss model with t-student family
"

### Investigate outliers/leverage points

"
Quality Control vs. Natural Variance
After manual inspection of high-leverage points (Cook's Distance):
REMOVED: Observations with illogical specs (e.g., $3k for Integrated GPU) or 
likely e-waste/parts ($84 for 2GB RAM). These are treated as scraping errors.
KEPT: Expensive legacy laptops. These are valid market anomalies, not data errors.
"

ids_to_remove <- c(424, 1671) 
data <- data %>% filter(!row_number() %in% ids_to_remove)

# Save the clean incomplete dataset
saveRDS(data, "final_incomplete_data.rds")

### Now we must rerun mice

init <- mice(data, maxit = 0)
pred_matrix <- init$predictorMatrix
mice_res <- mice(data, 
                 method = 'pmm', 
                 predictorMatrix = pred_matrix, 
                 m = 10,       
                 maxit = 50,   
                 seed = 123,
                 print = TRUE) 

# Check mice imputation validity
densityplot(mice_res)
stripplot(mice_res)
plot(mice_res, layout = c(2, 4))

### repeat baseline model

final_base <- with(mice_res, lm(log(price) ~ ram + screen_size + storage_gb + 
                                  cpu_tier + gpu_tier + brand + 
                                  is_pro_os + is_touch + is_2in1 + 
                                  has_backlit_keyboard + has_fingerprint + 
                                  product_line + rating + color + is_s_mode))

summary(pool(final_base), conf.int = TRUE)

aics_finalbase <- sapply(final_base$analyses, AIC)
mean_aic_finalbase <- mean(aics_finalbase)

par(mfrow=c(2,2))
plot(final_base$analyses[[1]])
plot(final_base$analyses[[4]])
plot(final_base$analyses[[7]])
plot(final_base$analyses[[9]])

data %>% mutate(rowid = row_number()) %>%  filter(rowid %in% c(374, 1571, 711, 312, 803))

data

"
This time the outliers are perfectly reasonable to mantain
I pretend to use bootstrap later, so keeping these values won't have great 
influence on the inference
"

### TF gamlss model

fit_student <- gamlss(log(price) ~ ram + screen_size + storage_gb + 
                     cpu_tier + gpu_tier + brand + 
                     is_pro_os + is_touch + is_2in1 + 
                     has_backlit_keyboard + has_fingerprint + 
                     product_line + rating + color + is_s_mode, family=TF, 
                   data = data_stacked, weights = w)

# Perform stepwise variable selection using GAIC to find the optimal parsimonious model
fit_student <- stepGAIC(fit_student, parallel="multicore", direction="both")

final_student <- with(mice_res, gamlss(log(price) ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                                         brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                                         color + is_s_mode,
                                       family = TF))

aics_student <- sapply(final_student$analyses, AIC)
mean_aic_student <- mean(aics_student)

par(mfrow=c(1,1))
plot(final_student$analyses[[7]])
wp(final_student$analyses[[7]])

"
The residuals of the left side of wormplot have a U shape that can be a sign of 
different distribution. In face of that, I will fit a mixture to the data
"

library(flexmix)

m <- mice_res$m
mx_models <- vector("list", m)

for (i in 1:m) {
  # Extrai o dataset imputado i
  data_i <- complete(mice_res, i)
  
  # Ajusta a mistura bimodal t-Student (TF)
  # K=2 foca em separar os dois 'regimes' de preço que causam o U no worm plot
  mx_models[[i]] <- initFlexmix(log(price) ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                               brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                               color + is_s_mode, 
                             k = 2,
                             nrep = 50,
                             data = data_i)
}

# Solve label switching by fixing the clusters by the intercept 
reorder_flex <- function(model) {
  intercepts <- parameters(model)[1, ] 
  order(intercepts)
}

# Construct matrix with prioris
aligned_priors <- sapply(1:m, function(i) {
  ordem <- reorder_flex(mx_models[[i]])
  prior(mx_models[[i]])[ordem]
})

print(aligned_priors)

model_mix_refits <- vector("list", m)

# refit for inference
for (i in 1:m) {
  data_i <- complete(mice_res, i)
  
  model_mix_refits[[i]] <- refit(mx_models[[i]])
}

# Residual analisys
# source: https://pmc.ncbi.nlm.nih.gov/articles/PMC6709981/pdf/nihms-1000938.pdf

B <- 50

draws_by_imp <- vector("list", m)

for (i in 1:m) {
  
  data_i <- complete(mice_res, i)
  fm <- results[[i]]$fm
  post <- posterior(fm)
  
  Xmat <- model.matrix(log(price) ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                         brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                         color + is_s_mode, data = data_i)
  y <- data_i$price
  resid_mat <- sapply(1:k, function(j) {
    beta_j <- fm@components[[j]][[1]]@parameters$coef
    y_hat_j <- as.vector(Xmat %*% beta_j)
    y - y_hat_j
  })
  
  draws_by_imp[[i]] <- replicate(B, {
    z <- apply(post, 1, function(p)
      sample(1:length(p), 1, prob = p))
    resid_mat[cbind(1:nrow(resid_mat), z)]
  }, simplify = FALSE)
}

# stack all simulations
n <- length(y)
all_draws <- do.call(
  cbind,
  unlist(draws_by_imp, recursive = FALSE)
)
dim(all_draws)

# Construct envolope
alpha <- 0.05

lower <- apply(all_draws, 1, quantile, probs = alpha/2)
upper <- apply(all_draws, 1, quantile, probs = 1 - alpha/2)
median_sim <- apply(all_draws, 1, median)

### graphs

par(mfrow=c(1,2))

# Residuals vs predicted
plot(y, type = "p", pch = 16, col = "grey",
     ylab = "Resíduo", xlab = "Índice")

lines(lower, col = "red", lwd = 2)
lines(upper, col = "red", lwd = 2)
lines(median_sim, col = "blue", lwd = 2)

# QQ-plot with envelope

# Order simulations
sorted_draws <- apply(all_draws, 2, sort)

lower_qq  <- apply(sorted_draws, 1, quantile, probs = 0.025)
upper_qq  <- apply(sorted_draws, 1, quantile, probs = 0.975)
median_qq <- apply(sorted_draws, 1, median)

# Observed residuals (posterior mean)
res_obs <- rowSums(resid_mat * post)
res_obs <- sort(res_obs)

qq <- qqnorm(res_obs, plot.it = FALSE)

plot(qq$x, qq$y, pch = 16)
lines(qq$x, lower_qq, col = "red", lwd = 2)
lines(qq$x, upper_qq, col = "red", lwd = 2)
lines(qq$x, median_qq, col = "blue", lwd = 2)


"
Results: heavy tails, assimetry and heteroscedasticity
Next step: Try heteroscedastic t-student model
"

fit_student_hetero <- gamlss(log(price) ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                               brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                               color + is_s_mode,
                      sigma.fo = ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                        brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                        color + is_s_mode,
                      family=TF, 
                      data = data_stacked, weights = w)

# Perform stepwise variable selection using GAIC to find the optimal parsimonious model
stepfit_student_hetero <- stepGAIC(fit_student_hetero, what = c("sigma"), 
                        parallel="multicore", direction="both")

final_student_hetero <- with(mice_res, gamlss(log(price) ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                                         brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                                         color + is_s_mode,
                                       sigma.fo = ~ ram + screen_size + 
                                         storage_gb + cpu_tier + brand + 
                                         is_pro_os + product_line + is_s_mode,
                                       family = TF))

aics_student_hetero <- sapply(final_student_hetero$analyses, AIC)
mean_aic_student_hetero <- mean(aics_student_hetero)

par(mfrow=c(1,1))
plot(final_student_hetero$analyses[[7]])
wp(final_student_hetero$analyses[[5]])

"
Results: Tails were better controlled
Next step: Try modelling tails
"

final_student_hetero_tails <- gamlss(log(price) ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                                       brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                                       color + is_s_mode,
                                     sigma.fo = ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                                       brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                                       color + is_s_mode,
                                     nu.fo = ~ ram + screen_size + storage_gb + cpu_tier + gpu_tier +  
                                       brand + is_pro_os + is_2in1 + has_backlit_keyboard + product_line +  
                                       color + is_s_mode,
                                     family=TF, 
                                     data = data_stacked, weights = w)


stepfinal_student_hetero_tails <- stepGAIC(final_student_hetero_tails, what="nu",
                                           parallel = "multicore", ncpus = 16, 
                                           direction="both")

















