# lfx
LIFX cli tool written in Haskell

# Usage:

```bash
usage: lfx [ls | toggle :selector | + :percent | - :percent | on | off]
```

# Configure:
To manage your lights you need to [generate](https://cloud.lifx.com/settings) and set lifx token:
```bash
$ lfx token <YOUR_TOKEN_HERE>
LIFX token written to ~/.lfxtoken
```

# Compile:
- Install [stack](https://docs.haskellstack.org/en/stable/README/)
- Clone and build this project:
```bash
$ git clone https://github.com/23ua/lfx
$ cd lfx
$ stack build

# to install binary:
$ stack install

# to run:
$ lfx
```

# Examples:
#### List lights:
```bash
$ lfx ls
d071d111812e :: Hall :: 15%
d071d1142312 :: Bedroom :: 5%

# format:
# id :: label :: brightness
```

#### Increase/decrease brightness:
```bash
# +10%
$ lfx + 10

# -5%
$ lfx - 5
```

#### On/off lights:
```bash
# on/off all lights:
$ lfx on
$ lfx off

# or by label:
$ lfx off label:Bedroom

# or even id:
$ lfx on d071d1142312
```

#### Toggle lights:
```bash
$ lfx toggle
$ lfx toggle label:Bedroom
$ lfx toggle d071d1142312
```
