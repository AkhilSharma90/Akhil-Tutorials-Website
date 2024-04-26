---
title: OSINT ( Google operators list )
date: 2023-12-18 15:01:35 +0300
draft: false
image: 'https://i.pinimg.com/originals/9d/f5/57/9df557ca8e79431172966376e32aa753.png'
---

| Search operator | What it does | Example |
| --- | --- | --- |
| “ ” | Search for results that mention a word or phrase. | https://www.google.com/search?q=%22steve+jobs%22 |
| OR | Search for results related to X or Y. | https://www.google.com/search?&q=jobs+OR+gates |
| | | Same as OR: | https://www.google.com/search?q=jobs%7Cgates |
| AND | Search for results related to X and Y. | https://www.google.com/search?&q=jobs+AND+gates |
| - | Search for results that don’t mention a word or phrase. | https://www.google.com/search?q=jobs+-apple |
| * | Wildcard matching any word or phrase. | https://www.google.com/search?q=%22steve+*+apple%22 |
| ( ) | Group multiple searches. | https://www.google.com/search?q=%28ipad+OR+iphone%29+apple |
| define: | Search for the definition of a word or phrase. | https://www.google.com/search?q=define%3Aentrepreneur |
| cache: | Find the most recent cache of a webpage. | https://webcache.googleusercontent.com/search?q=cache%3Aapple.com |
| filetype: | Search for particular types of files (e.g., PDF). | https://www.google.com/search?q=apple+filetype%3Apdf |
| ext: | Same as filetype: | https://www.google.com/search?q=apple+ext%3Apdf |
| site: | Search for results from a particular website. | https://www.google.com/search?q=site%3Aapple.com |
| related: | Search for sites related to a given domain. | https://www.google.com/search?q=related%3Aapple.com |
| intitle: | Search for pages with a particular word in the title tag. | https://www.google.com/search?q=intitle%3Aapple |
| allintitle: | Search for pages with multiple words in the title tag. | https://www.google.com/search?q=allintitle%3Aapple+iphone |
| inurl: | Search for pages with a particular word in the URL. | https://www.google.com/search?q=inurl%3Aapple |
| allinurl: | Search for pages with multiple words in the URL. | https://www.google.com/search?q=allinurl%3Aapple+iphone |
| intext: | Search for pages with a particular word in their content. | https://www.google.com/search?q=intext%3Aapple |
| allintext: | Search for pages with multiple words in their content. | https://www.google.com/search?q=allintext%3Aapple+iphone |
| weather: | Search for the weather in a location. | https://www.google.com/search?q=weather%3Asan+francisco |
| stocks: | Search for stock information for a ticker. | https://www.google.com/search?q=stocks%3Aaapl |
| map: | Force Google to show map results. | https://www.google.com/search?q=map%3Asilicon+valley |
| movie: | Search for information about a movie. | https://www.google.com/search?q=movie%3Asteve+jobs |
| in | Convert one unit to another. | https://www.google.com/search?q=$329+in+GBP |
| source: | Search for results from a particular source in Google News. | https://www.google.com/search?q=apple+source%3Athe_verge&tbm=nws |
| before: | Search for results from before a particular date. | https://www.google.com/search?q=apple+before%3A2007-06-29 |
| after: | Search for results from after a particular date. | https://www.google.com/search?q=apple+after%3A2007-06-29 |