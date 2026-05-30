import Quickshell
import Quickshell.Wayland
import Quickshell.Io
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

PanelWindow {
    id: root

    // Theme - CharmTone palette
    property color colBg: "#CC3A3943"
    property color colFg: "#D6D3DC"
    property color colMuted: "#605F6B"
    property color colCyan: "#A2A0AD"
    property color colBlue: "#BFBCC8"
    property color colYellow: "#BFBCC8"
    property color colCharple: "#6B50FF"
    property color colSquid: "#858392"
    property color colCoral: "#FF577D"
    property string fontFamily: "Maple Mono NF CN"
    property int fontSize: 14
    property string externalScreenName: "DP-2"
    property string deviceScreenName: "eDP-1"

    // System data
    property int cpuUsage: 0
    property int memUsage: 0
    property var lastCpuIdle: 0
    property var lastCpuTotal: 0
    property int batteryCapacity: 0
    property string batteryStatus: "Unknown"
    property int cpuTemp: 0
    property int gpuTemp: 0
    property real netRxRate: 0  // bytes/sec
    property real netTxRate: 0  // bytes/sec
    property var lastNetRx: -1
    property var lastNetTx: -1
    property var lastNetTime: 0
    property string wifiSsid: ""
    property int wifiSignal: 0
    property int diskUsage: 0
    property int volume: 0
    property bool volumeMuted: false
    property bool micMuted: true

    function fmtRate(bps) {
        var n, unit
        if (bps < 1024)                  { n = bps.toFixed(0);            unit = "B/s" }
        else if (bps < 1024 * 1024)      { n = (bps / 1024).toFixed(0);   unit = "K/s" }
        else if (bps < 1024 * 1024 * 1024) { n = (bps / 1048576).toFixed(1); unit = "M/s" }
        else                             { n = (bps / 1073741824).toFixed(2); unit = "G/s" }
        return n.padStart(5, "\u2007") + " " + unit
    }
    function ratePad(bps) {
        var n
        if (bps < 1024) n = bps.toFixed(0)
        else if (bps < 1024 * 1024) n = (bps / 1024).toFixed(0)
        else if (bps < 1024 * 1024 * 1024) n = (bps / 1048576).toFixed(1)
        else n = (bps / 1073741824).toFixed(2)
        return "\u00B7".repeat(Math.max(0, 5 - n.length))
    }
    function rateBody(bps) {
        var n, unit
        if (bps < 1024)                  { n = bps.toFixed(0);            unit = "B/s" }
        else if (bps < 1024 * 1024)      { n = (bps / 1024).toFixed(0);   unit = "K/s" }
        else if (bps < 1024 * 1024 * 1024) { n = (bps / 1048576).toFixed(1); unit = "M/s" }
        else                             { n = (bps / 1073741824).toFixed(2); unit = "G/s" }
        return n + " " + unit
    }
    function preferredScreen() {
        var screens = Quickshell.screens
        var external = screens.find(s => s.name === root.externalScreenName)
            ?? screens.find(s => s.name !== root.deviceScreenName)
        return external ?? screens.find(s => s.name === root.deviceScreenName) ?? screens[0]
    }

    // Processes and timers here...

    FileView {
        id: batCapFile
        path: "/sys/class/power_supply/BAT0/capacity"
        blockLoading: true
        onLoaded: root.batteryCapacity = parseInt(text(), 10) || 0
    }

    FileView {
        id: batStatusFile
        path: "/sys/class/power_supply/BAT0/status"
        blockLoading: true
        onLoaded: root.batteryStatus = text().trim()
    }

    Timer {
        interval: 15000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: { batCapFile.reload(); batStatusFile.reload() }
    }

    Process {
        id: tempProc
        command: ["sh", "-c",
            "c=0; g=0; for d in /sys/class/hwmon/hwmon*; do " +
            "n=$(cat \"$d/name\" 2>/dev/null); " +
            "case \"$n\" in " +
            "k10temp|coretemp|zenpower|cpu_thermal) " +
            "[ \"$c\" = 0 ] && c=$(cat \"$d/temp1_input\" 2>/dev/null);; " +
            "amdgpu|nvidia|i915|nouveau) " +
            "[ \"$g\" = 0 ] && g=$(cat \"$d/temp1_input\" 2>/dev/null);; " +
            "esac; done; echo \"${c:-0} ${g:-0}\""
        ]
        stdout: StdioCollector {
            onStreamFinished: {
                var parts = this.text.trim().split(/\s+/)
                root.cpuTemp = Math.round((parseInt(parts[0], 10) || 0) / 1000)
                root.gpuTemp = Math.round((parseInt(parts[1], 10) || 0) / 1000)
            }
        }
    }

    Timer {
        interval: 5000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: if (!tempProc.running) tempProc.running = true
    }

    FileView {
        id: netFile
        path: "/proc/net/dev"
        blockLoading: true
        onLoaded: {
            var lines = text().split("\n")
            var rx = 0, tx = 0
            for (var i = 2; i < lines.length; i++) {
                var line = lines[i].trim()
                if (!line) continue
                var colon = line.indexOf(":")
                if (colon < 0) continue
                var iface = line.substring(0, colon).trim()
                if (iface === "lo" || iface.indexOf("docker") === 0 ||
                    iface.indexOf("veth") === 0 || iface.indexOf("br-") === 0) continue
                var parts = line.substring(colon + 1).trim().split(/\s+/)
                rx += parseInt(parts[0], 10) || 0
                tx += parseInt(parts[8], 10) || 0
            }
            var now = Date.now()
            if (root.lastNetRx >= 0 && root.lastNetTime > 0) {
                var dt = (now - root.lastNetTime) / 1000
                if (dt > 0) {
                    root.netRxRate = Math.max(0, (rx - root.lastNetRx) / dt)
                    root.netTxRate = Math.max(0, (tx - root.lastNetTx) / dt)
                }
            }
            root.lastNetRx = rx
            root.lastNetTx = tx
            root.lastNetTime = now
        }
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: netFile.reload()
    }

    Process {
        id: wifiProc
        command: ["sh", "-c",
            "nmcli -t -f IN-USE,SIGNAL,SSID dev wifi 2>/dev/null | " +
            "awk -F: '$1==\"*\"{print $2\"\\t\"$3; exit}'"
        ]
        stdout: StdioCollector {
            onStreamFinished: {
                var line = this.text.trim()
                if (!line) { root.wifiSignal = 0; root.wifiSsid = ""; return }
                var parts = line.split("\t")
                root.wifiSignal = parseInt(parts[0], 10) || 0
                root.wifiSsid = parts[1] || ""
            }
        }
    }

    Timer {
        interval: 5000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: if (!wifiProc.running) wifiProc.running = true
    }

    Process {
        id: diskProc
        command: ["sh", "-c", "df -P / 2>/dev/null | awk 'NR==2{gsub(\"%\",\"\",$5); print $5}'"]
        stdout: StdioCollector {
            onStreamFinished: root.diskUsage = parseInt(this.text.trim(), 10) || 0
        }
    }

    Timer {
        interval: 30000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: if (!diskProc.running) diskProc.running = true
    }

    Process {
        id: audioProc
        command: ["sh", "-c",
            "v=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ 2>/dev/null); " +
            "m=$(wpctl get-volume @DEFAULT_AUDIO_SOURCE@ 2>/dev/null); " +
            "vol=$(echo \"$v\" | awk '{print $2}'); " +
            "vmute=$(echo \"$v\" | grep -q MUTED && echo 1 || echo 0); " +
            "mmute=$(echo \"$m\" | grep -q MUTED && echo 1 || echo 0); " +
            "echo \"$vol $vmute $mmute\""
        ]
        stdout: StdioCollector {
            onStreamFinished: {
                var parts = this.text.trim().split(/\s+/)
                root.volume = Math.round((parseFloat(parts[0]) || 0) * 100)
                root.volumeMuted = parts[1] === "1"
                root.micMuted = parts[2] === "1"
            }
        }
    }

    Timer {
        interval: 1500
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: if (!audioProc.running) audioProc.running = true
    }

    screen: preferredScreen()

    anchors.top: true
    anchors.left: true
    anchors.right: true
    margins { top: 6; left: 9; right: 9}
    implicitHeight: 26
    color: "transparent"
    
    Rectangle {
        anchors.fill: parent
        anchors.topMargin: 0
        anchors.bottomMargin: 0
        radius: 8
        color: root.colBg
    }

    // Workspaces (true center)
    Row {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter
        spacing: 16
        Repeater {
            model: 9
            Text {
                property var ws: Hyprland.workspaces.values.find(w => w.id === index + 1)
                property bool isActive: Hyprland.focusedWorkspace?.id === (index + 1)
                text: index + 1
                color: isActive ? root.colCharple : (ws ? root.colBlue : root.colMuted)
                font { family: root.fontFamily; pixelSize: root.fontSize; weight: Font.Black }
                MouseArea {
                    anchors.fill: parent
                    onClicked: Hyprland.dispatch("workspace " + (index + 1))
                }
            }
        }
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: 8
        anchors.rightMargin: 8
        anchors.topMargin: 2
        anchors.bottomMargin: 2
        spacing: 16

        // CPU
        Row {
            spacing: 0
            property int filled: Math.min(4, Math.max(0, Math.round(root.cpuUsage / 25)))
            Text {
                text: "\uf4bc " + "|".repeat(parent.filled)
                color: root.colYellow
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: "\u00B7".repeat(4 - parent.filled)
                color: root.colSquid
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: root.cpuUsage + "%"
                color: root.colYellow
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
        }


        // CPU / GPU temp
        Text {
            text: "\uf2c8 " + root.cpuTemp + "/" + root.gpuTemp + "\u00B0C"
            color: (root.cpuTemp >= 85 || root.gpuTemp >= 85) ? root.colCharple : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }


        // Memory
        Row {
            spacing: 0
            property int filled: Math.min(4, Math.max(0, Math.round(root.memUsage / 25)))
            Text {
                text: "\ue266 " + "|".repeat(parent.filled)
                color: root.colCyan
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: "\u00B7".repeat(4 - parent.filled)
                color: root.colSquid
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: root.memUsage + "%"
                color: root.colCyan
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
        }


        // Disk (/)
        Row {
            spacing: 0
            property int filled: Math.min(4, Math.max(0, Math.round(root.diskUsage / 25)))
            property color sectionCol: root.diskUsage >= 90 ? root.colCharple : root.colFg
            Text {
                text: "\uf0a0 " + "|".repeat(parent.filled)
                color: parent.sectionCol
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: "\u00B7".repeat(4 - parent.filled)
                color: root.colSquid
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: root.diskUsage + "%"
                color: parent.sectionCol
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
        }


        // Network I/O
        Row {
            spacing: 0
            Text {
                text: "\u2193"
                color: root.colFg
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: root.ratePad(root.netRxRate)
                color: root.colSquid
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: root.rateBody(root.netRxRate) + " \u2191"
                color: root.colFg
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: root.ratePad(root.netTxRate)
                color: root.colSquid
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
            Text {
                text: root.rateBody(root.netTxRate)
                color: root.colFg
                font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            }
        }

        Item { Layout.fillWidth: true }

        // WiFi
        Text {
            text: {
                if (root.wifiSignal <= 0 || !root.wifiSsid) return "\uf6ac off"
                return "\uf1eb\u2003" + root.wifiSsid + " " + root.wifiSignal + "%"
            }
            color: root.wifiSignal <= 0 ? root.colMuted : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }


        // Volume
        Text {
            id: volText
            text: {
                var g
                if (root.volumeMuted || root.volume <= 0) g = "\uf026"
                else if (root.volume < 50) g = "\uf027"
                else g = "\uf028"
                return g + "\u2009" + root.volume + "%"
            }
            color: root.volumeMuted ? root.colSquid : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            MouseArea {
                anchors.fill: parent
                cursorShape: Qt.PointingHandCursor
                onClicked: volMuteProc.running = true
            }
        }

        Process {
            id: volMuteProc
            command: ["wpctl", "set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"]
            onExited: audioProc.running = true
        }


        // Mic
        Text {
            id: micText
            TextMetrics { id: micOn;  font: micText.font; text: "\uf130" }
            TextMetrics { id: micOff; font: micText.font; text: "\uf131" }
            Layout.preferredWidth: Math.ceil(Math.max(micOn.advanceWidth, micOff.advanceWidth))
            horizontalAlignment: Text.AlignLeft
            text: root.micMuted ? "\uf131" : "\uf130"
            color: root.micMuted ? root.colCoral : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
            MouseArea {
                anchors.fill: parent
                cursorShape: Qt.PointingHandCursor
                onClicked: micMuteProc.running = true
            }
        }

        Process {
            id: micMuteProc
            command: ["wpctl", "set-mute", "@DEFAULT_AUDIO_SOURCE@", "toggle"]
            onExited: audioProc.running = true
        }


        // Clock
        Text {
            id: clock
            property var zh: Qt.locale("zh_CN")
            property bool showDate: true
            function fmt() {
                return showDate
                    ? zh.toString(new Date(), "ddd MMMd\u65E5 HH:mm")
                    : zh.toString(new Date(), "HH:mm")
            }
            color: root.colBlue
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: false}
            text: fmt()
            Timer {
                interval: 1000
                running: true
                repeat: true
                onTriggered: clock.text = clock.fmt()
            }
            MouseArea {
                anchors.fill: parent
                cursorShape: Qt.PointingHandCursor
                onClicked: { clock.showDate = !clock.showDate; clock.text = clock.fmt() }
            }
        }

        // Battery
        Text {
            text: {
                var glyph
                if (root.batteryStatus === "Charging") glyph = "\uf0e7"
                else if (root.batteryCapacity >= 88) glyph = "\uf240"
                else if (root.batteryCapacity >= 63) glyph = "\uf241"
                else if (root.batteryCapacity >= 38) glyph = "\uf242"
                else if (root.batteryCapacity >= 13) glyph = "\uf243"
                else glyph = "\uf244"
                return glyph + " " + root.batteryCapacity + "%"
            }
            color: root.batteryCapacity <= 15 && root.batteryStatus !== "Charging"
                ? root.colCharple : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }
    }
}
