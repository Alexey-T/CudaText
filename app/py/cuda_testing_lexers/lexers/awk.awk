# process_inventory.awk - A comprehensive AWK script example

# 1. BEGIN block: Executed once before processing any input lines.
BEGIN {
    # Set Field Separator (FS) and Output Field Separator (OFS).
    # -F option on command line is often preferred for FS for simple cases.
    FS = ","         # Input fields are separated by comma
    OFS = "\t"       # Output fields will be separated by a tab

    # Initialize variables.
    total_active_value = 0
    num_active_items = 0
    
    # Declare an associative array for category counts.
    # Arrays in AWK are associative (hash maps).
    # Keys can be strings or numbers.
    category_counts["Electronics"] = 0
    category_counts["Furniture"] = 0
    category_counts["Food"] = 0

    # Print a header for the report.
    print "--- Inventory Report ---"
    print "Processed at:", strftime("%Y-%m-%d %H:%M:%S") # strftime for current time
    print "------------------------"
    print "" # Blank line
}

# 2. Pattern { Action } blocks: Executed for each line that matches the pattern.

# Skip the header line (first line) using NR (Number of Record/Line).
NR == 1 {
    next # Skip to the next input line
}

# Process lines where the status is "Active".
$5 == "Active" {
    # Access fields using $1, $2, etc. $0 refers to the entire line.
    # $1: ID, $2: Name, $3: Category, $4: Value, $5: Status, $6: Notes

    # Accumulate total value for active items.
    total_active_value += $4
    num_active_items++

    # Increment category count for active items.
    category_counts[$3]++

    # Print details of active items.
    # printf for formatted output (like C's printf).
    printf "Active Item: %s (ID: %s) - $%.2f\n", $2, $1, $4
}

# Process lines where the category is "Electronics" and value is greater than 100.
# Patterns can be combined with logical operators (&&, ||, !).
$3 == "Electronics" && $4 > 100 {
    print "High-Value Electronics:", $2, "($"$4")"
}

# Use regular expressions as patterns.
# Print lines where "Broken" appears in the Notes field.
$6 ~ /Broken/ {
    print "Broken Item Found:", $2, "with notes:", $6
}

# Default action: If no pattern is specified, the action applies to all lines.
# This block is executed for all lines (except the header, due to `next` above).
# Demonstrate string functions.
{
    item_name_length = length($2) # length of the Name field
    # substr(string, start, length)
    # tolower(string), toupper(string)
    # index(string, substring)
    # match(string, regexp)
    # split(string, array, separator)
    # sub(regexp, replacement, string) - substitutes first match
    # gsub(regexp, replacement, string) - substitutes all matches (global)

    if (item_name_length > 10) {
        printf "Long name item: %s (Length: %d)\n", tolower($2), item_name_length
    }
}


# 3. END block: Executed once after processing all input lines.
END {
    print "\n--- Summary ---"
    if (num_active_items > 0) {
        printf "Total Active Items: %d\n", num_active_items
        printf "Sum of Active Item Values: $%.2f\n", total_active_value
        printf "Average Active Item Value: $%.2f\n", total_active_value / num_active_items
    } else {
        print "No active items found."
    }

    print "\nCategory Breakdown (Active Items):"
    # Looping through an associative array.
    # `for (key in array)` iterates over keys.
    for (category in category_counts) {
        printf "%s: %d items\n", category, category_counts[category]
    }

    # Built-in variables in END block
    print "\nProcessed", NR-1, "data records from file:", FILENAME
    print "Max fields per record (NF):", NF_max # Assuming NF_max was tracked, but it's per line, so this would be from the last line.
                                              # Better to store max NF in a variable if needed across entire file.
}

# Optional: To track max NF
# { if (NF > NF_max) NF_max = NF } # This needs to be in a pattern {action} block