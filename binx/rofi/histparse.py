#!/usr/bin/env python3

# sepre = ': 1485098437:0;'
sepre = '\n: \d{10}:\d;'
match = re.search(rf'{sepre}(.*){sepre}',
                  paragraph,
                  re.DOTALL)

