s <- searchTwitter('#emoticons', cainfo="cacert.pem")
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
require('streamR')
