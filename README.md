# lfx
LIFX cli tool written in Haskell

# Usage:

```bash
Usage: lfx [-s|--selector SELECTOR] COMMAND
  You can control your lights via lan protocol or via internet

Available options:
  -s,--selector SELECTOR   Apply command to lights matching the SELECTOR
  -h,--help                Show this help text

Available commands:
  on                       Turn on the lights
  off                      Turn off the lights
  toggle                   Toggle the lights
  ls                       List the lights
  brightness               Set the brightness of the lights
  +                        Increase the brightness of the lights by PERCENT
  -                        Decrease the brightness of the lights by PERCENT
  token                    Set the LIFX token
```

# Configure:
To manage your lights you need to [generate](https://cloud.lifx.com/settings) and set lifx token first:
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

#### Set lights brightness:
```bash
# Set to 10%
$ lfx brightness 10

# Set to 100%
$ lfx brightness 100
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
