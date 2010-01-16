## Copyright (c) 2007, Ryan Crum
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer.
##     * Redistributions in binary form must reproduce the above copyright
##       notice, this list of conditions and the following disclaimer in the
##       documentation and/or other materials provided with the distribution.
##     * The name of Ryan Crum may not be used to endorse or promote products
##       derived from this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY RYAN CRUM ``AS IS'' AND ANY
## EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL RYAN CRUM BE LIABLE FOR ANY
## DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

from PIL import Image
import math
import sys

if len(sys.argv) > 1:
    op = open(sys.argv[1])
else:
    op = sys.stdin

pixels = op.readlines()
op.close()

dimensions = int(math.ceil(math.sqrt(len(pixels))))
print dimensions

img = Image.new('RGB', (dimensions, dimensions))

for pixel in pixels:
    pixel = pixel.replace('}', '').replace('{', '').split(',')
    #print int(float(pixel[1])), int(float(pixel[3]))
    img.putpixel((int(float(pixel[1])), int(float(pixel[3]))), (int(float(pixel[5])), int(float(pixel[6])), int(float(pixel[7]))))

img.save('output.png', 'PNG')

print 'hello'
