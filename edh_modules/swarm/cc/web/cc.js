/**
 * Swarm Control Center
 */


const pageEpoch = Date.now()

function startEditTextArea(ta) {
  const cfe = ta.closest("div.CfgEdit")
  for (let btn of cfe.querySelectorAll("button")) {
    btn.disabled = false
  }
  cfe.style.zIndex = Date.now() - pageEpoch

  const pst = cfe.style
  pst.width = "" + Math.max(640, Math.min(1200, 36 + ta.scrollWidth)) + "px"
  pst.height = "" + Math.max(200, Math.min(700, 36 + ta.scrollHeight)) + "px"
  pst.position = "fixed"

  ta.readOnly = false
  ta.focus()
}

function stopEditTextArea(ta) {
  const cfe = ta.closest("div.CfgEdit")
  for (let btn of cfe.querySelectorAll("button")) {
    btn.disabled = true
  }
  cfe.style.zIndex = 1

  const pst = cfe.style
  pst.width = "auto"
  pst.height = "auto"
  pst.position = "static"

  ta.readOnly = true
}


const cnodeTable = document.getElementById("cnode_tbl")

// double click on textareas
cnodeTable.addEventListener("dblclick", async function (evt) {
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

  const mac = ta.dataset.mac
  const resp = await fetch("/cnode/v1/load/" + mac)
  if (!resp.ok) {
    console.error("Config load failure:", resp)
    alert("Failed refreshing config: " + resp.status)
    return
  }
  const result = await resp.json()
  if (result.err) {
    console.error("Failed refreshing config:", result)
    alert(result.err)
    return
  }
  ta.dataset.preEdit = ta.value = result['src']

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
        const mac = ta.dataset.mac
        const resp = await fetch("/cnode/v1/save/" + mac, {
          method: "POST",
          body: JSON.stringify({
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
        ta.dataset.preEdit = ta.value

        stopEditTextArea(ta)
      }
      break
    case "cancel":
      for (let ta of cfe.querySelectorAll("textarea")) {
        ta.value = ta.dataset.preEdit
        stopEditTextArea(ta)
      }
      break
    case "reboot":
      const ip = btn.dataset.ip, account = btn.dataset.account
      if (!window.confirm(`Confirm to reboot [${ip}] ?`)) return
      const resp = await fetch('/reboot/' + account)
      if (!resp.ok) {
        console.error("Reboot failure:", resp)
        alert("Failed rebooting [" + ip + "]: " + resp.status)
      }
      break
    default:
      console.warn("Unknown button action:", btn.dataset.name, btn)
  }
})
