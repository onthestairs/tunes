module Notes where

import Relude

newtype Note = Note {unNote :: Word8}
  deriving newtype (Ord, Eq, Show, Num)

c1 :: Note
c1 = Note 99

c'1 :: Note
c'1 = Note 100

d1 :: Note
d1 = Note 101

d'1 :: Note
d'1 = Note 102

e1 :: Note
e1 = Note 103

f1 :: Note
f1 = Note 104

f'1 :: Note
f'1 = Note 105

g1 :: Note
g1 = Note 106

g'1 :: Note
g'1 = Note 107

a1 :: Note
a1 = Note 108

a'1 :: Note
a'1 = Note 109

b1 :: Note
b1 = Note 110

c2 :: Note
c2 = Note 111

c'2 :: Note
c'2 = Note 112

d2 :: Note
d2 = Note 113

d'2 :: Note
d'2 = Note 114

e2 :: Note
e2 = Note 115

f2 :: Note
f2 = Note 116

f'2 :: Note
f'2 = Note 117

g2 :: Note
g2 = Note 118

g'2 :: Note
g'2 = Note 119

a2 :: Note
a2 = Note 120

a'2 :: Note
a'2 = Note 121

b2 :: Note
b2 = Note 122

c3 :: Note
c3 = Note 123

c'3 :: Note
c'3 = Note 124

d3 :: Note
d3 = Note 125

d'3 :: Note
d'3 = Note 126

e3 :: Note
e3 = Note 127

f3 :: Note
f3 = Note 128

f'3 :: Note
f'3 = Note 129

g3 :: Note
g3 = Note 130

g'3 :: Note
g'3 = Note 131

a3 :: Note
a3 = Note 132

a'3 :: Note
a'3 = Note 133

b3 :: Note
b3 = Note 134

c4 :: Note
c4 = Note 135

c'4 :: Note
c'4 = Note 136

d4 :: Note
d4 = Note 137

d'4 :: Note
d'4 = Note 138

e4 :: Note
e4 = Note 139

f4 :: Note
f4 = Note 140

f'4 :: Note
f'4 = Note 141

g4 :: Note
g4 = Note 142

g'4 :: Note
g'4 = Note 143

a4 :: Note
a4 = Note 144

a'4 :: Note
a'4 = Note 145

b4 :: Note
b4 = Note 146

c5 :: Note
c5 = Note 147

c'5 :: Note
c'5 = Note 148

d5 :: Note
d5 = Note 149

d'5 :: Note
d'5 = Note 150

e5 :: Note
e5 = Note 151

f5 :: Note
f5 = Note 152

f'5 :: Note
f'5 = Note 153

g5 :: Note
g5 = Note 154

g'5 :: Note
g'5 = Note 155

a5 :: Note
a5 = Note 156

a'5 :: Note
a'5 = Note 157

b5 :: Note
b5 = Note 158

c6 :: Note
c6 = Note 159

c'6 :: Note
c'6 = Note 160

d6 :: Note
d6 = Note 161

d'6 :: Note
d'6 = Note 162

e6 :: Note
e6 = Note 163

f6 :: Note
f6 = Note 164

f'6 :: Note
f'6 = Note 165

g6 :: Note
g6 = Note 166

g'6 :: Note
g'6 = Note 167

a6 :: Note
a6 = Note 168

a'6 :: Note
a'6 = Note 169

b6 :: Note
b6 = Note 170

c7 :: Note
c7 = Note 171

c'7 :: Note
c'7 = Note 172

d7 :: Note
d7 = Note 173

d'7 :: Note
d'7 = Note 174

e7 :: Note
e7 = Note 175

f7 :: Note
f7 = Note 176

f'7 :: Note
f'7 = Note 177

g7 :: Note
g7 = Note 178

g'7 :: Note
g'7 = Note 179

a7 :: Note
a7 = Note 180

a'7 :: Note
a'7 = Note 181

b7 :: Note
b7 = Note 182

c8 :: Note
c8 = Note 183

c'8 :: Note
c'8 = Note 184

d8 :: Note
d8 = Note 185

d'8 :: Note
d'8 = Note 186

e8 :: Note
e8 = Note 187

f8 :: Note
f8 = Note 188

f'8 :: Note
f'8 = Note 189

g8 :: Note
g8 = Note 190

g'8 :: Note
g'8 = Note 191

a8 :: Note
a8 = Note 192

a'8 :: Note
a'8 = Note 193

b8 :: Note
b8 = Note 194

c9 :: Note
c9 = Note 195

c'9 :: Note
c'9 = Note 196

d9 :: Note
d9 = Note 197

d'9 :: Note
d'9 = Note 198

e9 :: Note
e9 = Note 199

f9 :: Note
f9 = Note 200

f'9 :: Note
f'9 = Note 201

g9 :: Note
g9 = Note 202

g'9 :: Note
g'9 = Note 203

a9 :: Note
a9 = Note 204

a'9 :: Note
a'9 = Note 205

b9 :: Note
b9 = Note 206

c10 :: Note
c10 = Note 207

c'10 :: Note
c'10 = Note 208

d10 :: Note
d10 = Note 209

d'10 :: Note
d'10 = Note 210

e10 :: Note
e10 = Note 211

f10 :: Note
f10 = Note 212

f'10 :: Note
f'10 = Note 213

g10 :: Note
g10 = Note 214

g'10 :: Note
g'10 = Note 215

a10 :: Note
a10 = Note 216

a'10 :: Note
a'10 = Note 217

b10 :: Note
b10 = Note 218
