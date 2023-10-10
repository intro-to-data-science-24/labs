
# 1.a.
# "\\$[0-9]+" will match one or more numbers followed by a dollar sign. For example, "\\$[0-9]+": c(c("$150", "$690", "$75").

# 1.b.
# "^.*$" will match any string. For example, "^.*$": c("dog", "$1.23", "lorem ipsum").

# 1.c.
# "\\d{4}-\\d{2}-\\d{2}" will match four digits followed by a hyphen, followed by two digits followed by a hyphen, followed by another two digits. This is a regular expression that can match dates formatted like “YYYY-MM-DD” (“%Y-%m-%d”). For example, "\d{4}-\d{2}-\d{2}": "2018-01-11"

# 1.d.
# ".*\\.txt$" will match any .txt file. For example, ".*\\.txt$": c("FiLeN4m3.txt", "notes.txt", "1982-10-23.txt")

# 1.e. 
# "\\\\{4}" is \\{4}, which will match four backslashes. For example, "\\\\{4}": "\\\\\\\\"

# 1.f.
# "b[a-z]{1,4}" will match the letter 'b' followed by three letters. For example, "b[a-z]{1,4}": c("band", "barn", "auburn")

# 2.
str_detect(stringr::words, '^\\w{2}y$') |> sum()

# 3.
str_subset(stringr::words, "[^e]ed$")

# 4. 
emails <- c('456123@students.hertie-school.org', 'h.simpson@students.hertie-school.org')
str_view(emails, '(^\\d{6}|^\\w\\.\\w{1,})@students\\.hertie-school\\.org')

# 5.
ex_string <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5553642Dr. Julius Hibbert"
str_view(ex_string, "[[:alpha:]., ]{2,}")
str_view(ex_string, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}")

# 6.
secret <- "clcopCow1zmstc0d87wnkig7OvdicpNuggvhryn92Gjuwczi8hqrfpRxs5Aj5dwpn0TanwoUwisdij7Lj8kpf03AT5Idr3coc0bt7yczjatOaootj55t3Nj3ne6c4Sfek.r1w1YwwojigOd6vrfUrbz2.2bkSnbhzgv4O9i05zLcropwVgnbEqoD65fa1otf.b7wIm24k6t3s9zqe5fy89n6Td5t9kc4f905gmc4gxo5nhk!gr"

solved <- unlist(str_extract_all(secret, "[[:upper:][:punct:]]"))
str_c(solved, collapse = "")
