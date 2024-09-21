.moment
| strptime("%Y-%m-%d")? // strptime("%Y-%m-%dT%T")
| mktime
| . + 1000000000
| strftime("%Y-%m-%dT%T")
