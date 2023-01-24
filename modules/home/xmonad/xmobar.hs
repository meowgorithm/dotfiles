Config
    { font     = "JetBrains Mono Medium 9"
    , bgColor  = "#262626"
    , alpha    = 216
    , fgColor  = "#777777"
    , position = TopSize C 100 42
    , commands = [ Run Date "星期%u %Y年%m月%d日 %H:%M" "date" 600
                 , Run Wireless "wlp76s0" [ "-x", "", "-t", "<essid>" ] 100
                 , Run StdinReader
                 , Run Network "enp78s0" [ "-S", "True", "-t", "Eth: <fc=#4eb4fa>↓<rx></fc> <fc=#4eb4fa>↑<tx></fc>" ] 10
                 , Run DiskU [ ("/", "Disk: <used>/<size>"), ("/boot", "(Boot: <used>/<size>)") ] [] 60
                 , Run Cpu [ "-t", "CPU: <fc=#4eb4fa><bar> <total>%</fc>" ] 10
                 , Run Uptime ["-t", "Up: <days>d <hours>h <minutes>m"] 36000
                 , Run Memory [ "-t", "Mem: <fc=#4eb4fa><usedbar> <usedratio>%</fc>" ] 10
                 ]
    , sepChar  = "%"
    , alignSep = "}{"
    , template = " %StdinReader% } <fc=#505050>%date%</fc> { %enp78s0%   %disku%   %cpu%   %memory%   %uptime% "
}