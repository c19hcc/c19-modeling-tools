import sys

if len(sys.argv) > 1:
    txt = 'printing {}'.format(sys.argv[1])
else:
    txt = 'no arguments passed!'
print(txt)
