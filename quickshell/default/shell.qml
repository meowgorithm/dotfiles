import Quickshell
import Quickshell.Wayland
import Quickshell.Io
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

PanelWindow {
    id: root

    // Theme - CharmTone palette
    property color colBg: "#2D2C36"       // BBQ (neutral dark)
    property color colFg: "#D6D3DC"       // Steep (neutral light)
    property color colMuted: "#605F6B"    // Oyster (neutral mid)
    property color colCyan: "#A2A0AD"     // Steam (neutral)
    property color colBlue: "#BFBCC8"     // Smoke (neutral)
    property color colYellow: "#BFBCC8"   // Smoke (neutral)
    property color colCharple: "#6B50FF"  // Charple (primary)
    property string fontFamily: "JetBrains Mono"
    property int fontSize: 14

    // System data
    property int cpuUsage: 0
    property int memUsage: 0
    property var lastCpuIdle: 0
    property var lastCpuTotal: 0

    // Processes and timers here...

    anchors.top: true
    anchors.left: true
    anchors.right: true
    margins { top: 8; left: 8; right: 8}
    implicitHeight: 30
    color: "transparent"
    
    Rectangle {
        anchors.fill: parent
        radius: 5
        color: root.colBg
    }

    RowLayout {
        anchors.fill: parent
        anchors.margins: 8
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

        // Memory
        Text {
            text: "Mem " + memUsage + "%"
            color: root.colCyan
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
