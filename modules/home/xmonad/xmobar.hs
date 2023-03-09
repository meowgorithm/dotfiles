Config
    { font     = "JetBrains Mono Medium 9"
    , bgColor  = "#262626"
    , alpha    = 255
    , fgColor  = "#777777"
    , position = TopSize C 100 42
    , commands = [ Run Date "星期%u %Y年%m月%d日 %H:%M" "date" 600
                 , Run Wireless "wlp76s0" [ "-x", "", "-t", "<essid>" ] 100
                 , Run StdinReader
                 , Run Network "enp78s0" [ "-S", "True", "-t", "Eth: <fc=#5858d5>↓<rx></fc> <fc=#5858d5>↑<tx></fc>" ] 10
                 , Run DiskU [ ("/", "/: <fc=#5858d5><used>/<size></fc>"), ("/boot", "/boot: <fc=#5858d5><used>/<size></fc>") ] [] 60
                 , Run Cpu [ "-t", "CPU: <fc=#5858d5><bar> <total>%</fc>" ] 10
                 , Run Uptime ["-t", "Up: <days>d <hours>h <minutes>m"] 36000
                 , Run Memory [ "-t", "Mem: <fc=#5858d5><usedbar> <usedratio>%</fc>" ] 10
                 ]
    , sepChar  = "%"
    , alignSep = "}{"
    , template = " %StdinReader% } <fc=#777777>%date%</fc> { %enp78s0%   %disku%   %cpu%   %memory%   %uptime% "
}