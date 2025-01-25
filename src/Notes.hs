module Notes where

import Relude

newtype Note = Note {unNote :: Word8}
  deriving newtype (Ord, Eq, Show, Num)

c1 :: Note
c1 = Note 99

cs1 :: Note
cs1 = Note 100

d1 :: Note
d1 = Note 101

ds1 :: Note
ds1 = Note 102

e1 :: Note
e1 = Note 103

f1 :: Note
f1 = Note 104

fs1 :: Note
fs1 = Note 105

g1 :: Note
g1 = Note 106

gs1 :: Note
gs1 = Note 107

a1 :: Note
a1 = Note 108

as1 :: Note
as1 = Note 109

b1 :: Note
b1 = Note 110

c2 :: Note
c2 = Note 111

cs2 :: Note
cs2 = Note 112

d2 :: Note
d2 = Note 113

ds2 :: Note
ds2 = Note 114

e2 :: Note
e2 = Note 115

f2 :: Note
f2 = Note 116

fs2 :: Note
fs2 = Note 117

g2 :: Note
g2 = Note 118

gs2 :: Note
gs2 = Note 119

a2 :: Note
a2 = Note 120

as2 :: Note
as2 = Note 121

b2 :: Note
b2 = Note 122

c3 :: Note
c3 = Note 123

cs3 :: Note
cs3 = Note 124

d3 :: Note
d3 = Note 125

ds3 :: Note
ds3 = Note 126

e3 :: Note
e3 = Note 127

f3 :: Note
f3 = Note 128

fs3 :: Note
fs3 = Note 129

g3 :: Note
g3 = Note 130

gs3 :: Note
gs3 = Note 131

a3 :: Note
a3 = Note 132

as3 :: Note
as3 = Note 133

b3 :: Note
b3 = Note 134

c4 :: Note
c4 = Note 135

cs4 :: Note
cs4 = Note 136

d4 :: Note
d4 = Note 137

ds4 :: Note
ds4 = Note 138

e4 :: Note
e4 = Note 139

f4 :: Note
f4 = Note 140

fs4 :: Note
fs4 = Note 141

g4 :: Note
g4 = Note 142

gs4 :: Note
gs4 = Note 143

a4 :: Note
a4 = Note 144

as4 :: Note
as4 = Note 145

b4 :: Note
b4 = Note 146

c5 :: Note
c5 = Note 147

cs5 :: Note
cs5 = Note 148

d5 :: Note
d5 = Note 149

ds5 :: Note
ds5 = Note 150

e5 :: Note
e5 = Note 151

f5 :: Note
f5 = Note 152

fs5 :: Note
fs5 = Note 153

g5 :: Note
g5 = Note 154

gs5 :: Note
gs5 = Note 155

a5 :: Note
a5 = Note 156

as5 :: Note
as5 = Note 157

b5 :: Note
b5 = Note 158

c6 :: Note
c6 = Note 159

cs6 :: Note
cs6 = Note 160

d6 :: Note
d6 = Note 161

ds6 :: Note
ds6 = Note 162

e6 :: Note
e6 = Note 163

f6 :: Note
f6 = Note 164

fs6 :: Note
fs6 = Note 165

g6 :: Note
g6 = Note 166

gs6 :: Note
gs6 = Note 167

a6 :: Note
a6 = Note 168

as6 :: Note
as6 = Note 169

b6 :: Note
b6 = Note 170

c7 :: Note
c7 = Note 171

cs7 :: Note
cs7 = Note 172

d7 :: Note
d7 = Note 173

ds7 :: Note
ds7 = Note 174

e7 :: Note
e7 = Note 175

f7 :: Note
f7 = Note 176

fs7 :: Note
fs7 = Note 177

g7 :: Note
g7 = Note 178

gs7 :: Note
gs7 = Note 179

a7 :: Note
a7 = Note 180

as7 :: Note
as7 = Note 181

b7 :: Note
b7 = Note 182

c8 :: Note
c8 = Note 183

cs8 :: Note
cs8 = Note 184

d8 :: Note
d8 = Note 185

ds8 :: Note
ds8 = Note 186

e8 :: Note
e8 = Note 187

f8 :: Note
f8 = Note 188

fs8 :: Note
fs8 = Note 189

g8 :: Note
g8 = Note 190

gs8 :: Note
gs8 = Note 191

a8 :: Note
a8 = Note 192

as8 :: Note
as8 = Note 193

b8 :: Note
b8 = Note 194

c9 :: Note
c9 = Note 195

cs9 :: Note
cs9 = Note 196

d9 :: Note
d9 = Note 197

ds9 :: Note
ds9 = Note 198

e9 :: Note
e9 = Note 199

f9 :: Note
f9 = Note 200

fs9 :: Note
fs9 = Note 201

g9 :: Note
g9 = Note 202

gs9 :: Note
gs9 = Note 203

a9 :: Note
a9 = Note 204

as9 :: Note
as9 = Note 205

b9 :: Note
b9 = Note 206

c10 :: Note
c10 = Note 207

cs10 :: Note
cs10 = Note 208

d10 :: Note
d10 = Note 209

ds10 :: Note
ds10 = Note 210

e10 :: Note
e10 = Note 211

f10 :: Note
f10 = Note 212

fs10 :: Note
fs10 = Note 213

g10 :: Note
g10 = Note 214

gs10 :: Note
gs10 = Note 215

a10 :: Note
a10 = Note 216

as10 :: Note
as10 = Note 217

b10 :: Note
b10 = Note 218
