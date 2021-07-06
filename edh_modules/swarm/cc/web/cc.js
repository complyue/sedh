/**
 * Swarm Control Center
 */

window.promptReboot = function promptReboot(ip, account) {
  if (!window.confirm(`Reboot [${ip}] ?`)) return
  window.alert('do reboot as: ' + account)
}


function startEditTextArea(ta) {
  ta.readOnly = false
  const cfe = ta.closest("div.CfgEdit")
  for (let btn of cfe.querySelectorAll("button")) {
    btn.disabled = false
  }

  const pst = cfe.style
  pst.width = "" + Math.max(640, Math.min(1200, 36 + ta.scrollWidth)) + "px"
  pst.height = "" + Math.max(200, Math.min(700, 36 + ta.scrollHeight)) + "px"
  pst.position = "fixed"
  ta.focus()
}

function stopEditTextArea(ta) {
  ta.readOnly = true
  const cfe = ta.closest("div.CfgEdit")
  for (let btn of cfe.querySelectorAll("button")) {
    btn.disabled = true
  }

  const pst = cfe.style
  pst.width = "auto"
  pst.height = "auto"
  pst.position = "static"
}

const cnodeTable = document.getElementById("cnode_tbl")

// double click on textareas
cnodeTable.addEventListener("dblclick", function (evt) {
  const ta = evt.target
  if ("TEXTAREA" !== ta.tagName) {
    return
  }
  if (!ta.readOnly) {
    // in editing mode, let it be default behavior
    return
  }

  // cease other behaviors for double click on readonly textarea
  evt.preventDefault()
  evt.stopImmediatePropagation()

  ta.dataset.preEdit = ta.value
  startEditTextArea(ta)
})

// button click
cnodeTable.addEventListener("click", async function (evt) {
  const btn = evt.target
  if ("BUTTON" != btn.tagName) {
    return
  }
  const cfe = btn.closest("div.CfgEdit")
  switch (btn.name) {
    case "save":
      for (let ta of cfe.querySelectorAll("textarea")) {
        try {
          const resp = await fetch("/cnode/v1/save", {
            method: "POST",
            body: JSON.stringify({
              mac: ta.dataset.mac,
              afterEdit: ta.value,
              preEdit: ta.dataset.preEdit
            }),
            headers: {
              "Content-Type": "application/json"
            }
          })
          if (!resp.ok) {
            console.error("Config save failure:", resp)
            alert("Failed saving config: " + resp.status)
            return
          }
          const result = await resp.json()
          if (result.err) {
            console.error("Failed saving config:", result)
            alert(result.err)
            return
          }
          stopEditTextArea(ta)
        } catch (err) {
          console.error("Error saving config:", err)
          alert("Failed saving config: " + err)
        }
      }
      break
    case "cancel":
      for (let ta of cfe.querySelectorAll("textarea")) {
        ta.value = ta.dataset.preEdit
        stopEditTextArea(ta)
      }
      break
    default:
      console.warn("Unknown button action:", btn.dataset.act, btn)
  }
})
