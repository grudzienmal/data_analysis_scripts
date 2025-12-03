# Install required packages if not installed
install.packages(c("readxl", "tidyverse", "ggpubr", "ggsci"))

# Load libraries
library(readxl)
library(tidyverse)
library(ggpubr)
library(ggsci)  # for npg color palette

# 1. Load the data
df <- read_excel("foci_count_test.xls")

# 2. Reshape to long format
df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "Group_Time", values_to = "Foci_Count") %>%
  separate(Group_Time, into = c("Group", "Time"), sep = " ") %>%
  mutate(Time = as.numeric(Time))

# 3. Define factor levels for x-axis (WT first, then MUT)
df_long <- df_long %>%
  mutate(Sample_Group = paste(Group, Time, sep = "_"),
         Sample_Group = factor(Sample_Group, levels = c("WT_0", "WT_0.5", "WT_4", "WT_8",
                                                        "MUT_0", "MUT_0.5", "MUT_4", "MUT_8")))

# 4. Create the plot with npg palette
p <- ggplot(df_long, aes(x = Sample_Group, y = Foci_Count, color = as.factor(Time))) +
  geom_jitter(width = 0.2, size = 2) +
  scale_color_npg(name = "Time (h)") +  # Apply NPG color palette
  theme_bw() +
  labs(title = "Foci Count Comparison", x = "Sample Group", y = "Foci Count")

# 5. Add statistical comparisons
comparisons <- list(c("WT_0.5", "MUT_0.5"), c("WT_4", "MUT_4"), c("WT_8", "MUT_8"))
p + stat_compare_means(comparisons = comparisons, method = "t.test")
