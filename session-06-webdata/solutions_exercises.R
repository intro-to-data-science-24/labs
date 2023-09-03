

1.
"^.*$" will match any string. For example: ^.*$: c("dog", "$1.23", "lorem ipsum").

"\\{.+\\}" will match any string with curly braces surrounding at least one character. For example: "\\{.+\\}": c("{a}", "{abc}").

"\\d{4}-\\d{2}-\\d{2}" will match four digits followed by a hyphen, followed by two digits followed by a hyphen, 
followed by another two digits. 
This is a regular expression that can match dates formatted like “YYYY-MM-DD” (“%Y-%m-%d”). 
For example: \d{4}-\d{2}-\d{2}: 2018-01-11

"\\\\{4}" is \\{4}, which will match four backslashes. For example: "\\\\{4}": "\\\\\\\\"


2.
str_detect(stringr::words, '^\\w{2}y$') %>% sum

3.
str_subset(stringr::words, "[^e]ed$")

4. 
str_view(hertie_emails, '(^\\d{6}|^\\w\\.\\w{1,})@students\\.hertie-school\\.org')

5.
str_view_all(raw_data, "[[:alpha:]., ]{2,}")
str_view_all(raw_data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}")
