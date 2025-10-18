#!/usr/bin/env python3
import re

def slugify(text):
    return re.sub(r'[^a-z0-9]+', '-', text.lower().strip()).strip('-')

print('CLEAROUTBUFFER ->', slugify('CLEAROUTBUFFER'))
print('14B CLEAROUTBUFFER ->', slugify('14B CLEAROUTBUFFER'))
print('ClearOutBuffer ->', slugify('ClearOutBuffer'))
print('14b-clearoutbuffer ->', slugify('14b-clearoutbuffer'))
