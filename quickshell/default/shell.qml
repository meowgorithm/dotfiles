import Quickshell
import Quickshell.Wayland
import Quickshell.Io
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

PanelWindow {
    id: root

    // Theme - CharmTone palette
    property color colBg: "transparent"
    property color colFg: "#D6D3DC"
    property color colMuted: "#605F6B"
    property color colCyan: "#A2A0AD"
    property color colBlue: "#BFBCC8"
    property color colYellow: "#BFBCC8"
    property color colCharple: "#6B50FF"
    property string fontFamily: "JetBrains Mono"
    property int fontSize: 14

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

    function fmtRate(bps) {
        var n, unit
        if (bps < 1024)                  { n = bps.toFixed(0);            unit = "B/s" }
        else if (bps < 1024 * 1024)      { n = (bps / 1024).toFixed(0);   unit = "K/s" }
        else if (bps < 1024 * 1024 * 1024) { n = (bps / 1048576).toFixed(1); unit = "M/s" }
        else                             { n = (bps / 1073741824).toFixed(2); unit = "G/s" }
        return n.padStart(5, "\u2007") + " " + unit
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

    anchors.top: true
    anchors.left: true
    anchors.right: true
    margins { top: 6; left: 9; right: 9}
    implicitHeight: 20
    color: "transparent"
    
    Rectangle {
        anchors.fill: parent
        radius: 3
        color: root.colBg
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: 8
        anchors.rightMargin: 8
        anchors.topMargin: 2
        anchors.bottomMargin: 2
        spacing: 8

        // Workspaces
        Repeater {
            model: 9
            Text {
                property var ws: Hyprland.workspaces.values.find(w => w.id === index + 1)
                property bool isActive: Hyprland.focusedWorkspace?.id === (index + 1)
                text: index + 1
                color: isActive ? root.colCharple : (ws ? root.colBlue : root.colMuted)
                font { family: root.fontFamily;pixelSize: root.fontSize; variableAxes: { "wght": 700 } }
                MouseArea {
                    anchors.fill: parent
                    onClicked: Hyprland.dispatch("workspace " + (index + 1))
                }
            }
        }

        Item { Layout.fillWidth: true }

        // CPU
        Text {
            text: "CPU " + cpuUsage + "%"
            color: root.colYellow
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }

        Rectangle { width: 1; height: 16; color: root.colMuted }

        // CPU temp
        Text {
            text: "CPU " + root.cpuTemp + "\u00B0C"
            color: root.cpuTemp >= 85 ? root.colCharple : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }

        Rectangle { width: 1; height: 16; color: root.colMuted }

        // GPU temp
        Text {
            text: "GPU " + root.gpuTemp + "\u00B0C"
            color: root.gpuTemp >= 85 ? root.colCharple : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }

        Rectangle { width: 1; height: 16; color: root.colMuted }

        // Memory
        Text {
            text: "Mem " + memUsage + "%"
            color: root.colCyan
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }

        Rectangle { width: 1; height: 16; color: root.colMuted }

        // Network I/O
        Text {
            text: "\u2193 " + root.fmtRate(root.netRxRate) + "  \u2191 " + root.fmtRate(root.netTxRate)
            color: root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }

        Rectangle { width: 1; height: 16; color: root.colMuted }

        // Battery
        Text {
            text: {
                var prefix = "Bat"
                if (root.batteryStatus === "Charging") prefix = "Chg"
                else if (root.batteryStatus === "Full") prefix = "Full"
                return prefix + " " + root.batteryCapacity + "%"
            }
            color: root.batteryCapacity <= 15 && root.batteryStatus !== "Charging"
                ? root.colCharple : root.colFg
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }
        }

        Rectangle { width: 1; height: 16; color: root.colMuted }

        // Clock
        Text {
            id: clock
            color: root.colBlue
            font { family: root.fontFamily; pixelSize: root.fontSize; bold: false}
            text: Qt.formatDateTime(new Date(), "ddd dd MMM HH:mm")
            Timer {
                interval: 1000
                running: true
                repeat: true
                onTriggered: clock.text = Qt.formatDateTime(new Date(), "ddd dd MMM HH:mm")
            }
        }
    }
}
