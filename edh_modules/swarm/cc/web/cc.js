/**
 * Control Center
 */

function getOffset(el) {
  var _x = 0;
  var _y = 0;
  while (el && !isNaN(el.offsetLeft) && !isNaN(el.offsetTop)) {
    _x += el.offsetLeft - el.scrollLeft;
    _y += el.offsetTop - el.scrollTop;
    el = el.offsetParent;
  }
  return { top: _y, left: _x };
}

function startEditTextArea(ta) {
  const { top, left } = getOffset(ta);
  const width = ta.scrollWidth;

  const tast = ta.style;

  tast.width = "" + width + "px";
  setTimeout(() => {
    const height = ta.scrollHeight;
    tast.position = "fixed";
    tast.height = "" + height + "px";
    tast.top = "" + (top + 16) + "px";
    tast.left = "" + left + "px";
  }, 1);
}

function stopEditTextArea(ta) {
  const tast = ta.style;

  tast.position = "";
  tast.width = "";
  tast.height = "";
  tast.top = "";
  tast.left = "";
}

const cnodeTable = document.getElementById("cnode_tbl");

// double click on textareas
cnodeTable.addEventListener("dblclick", function(evt) {
  const ta = evt.target;
  if ("TEXTAREA" !== ta.tagName) {
    return;
  }
  if (!ta.readOnly) {
    // in editing mode, let it be default behavior
    return;
  }

  if (ta.dataset.filename) {
    // only start edit if has a filename with it

    ta.dataset.preEdit = ta.value;
    ta.readOnly = false;
    const cfe = ta.closest("div.ConfigFileEdit");
    for (let btn of cfe.querySelectorAll("button")) {
      btn.disabled = false;
    }

    ta.blur();
    ta.focus();
    startEditTextArea(ta);
  }

  // cease other behaviors for double click on readonly textarea
  evt.preventDefault();
  evt.stopImmediatePropagation();
});

// button click
cnodeTable.addEventListener("click", async function(evt) {
  const btn = evt.target;
  if ("BUTTON" != btn.tagName) {
    return;
  }
  const cfe = btn.closest("div.ConfigFileEdit");
  switch (btn.dataset.act) {
    case "save":
      for (let ta of cfe.querySelectorAll("textarea")) {
        try {
          const resp = await fetch("/cnode/v1/save", {
            method: "POST",
            body: JSON.stringify({
              FileName: ta.dataset.filename,
              AfterEdit: ta.value,
              PreEdit: ta.dataset.preEdit
            }),
            headers: {
              "Content-Type": "application/json"
            }
          });
          if (!resp.ok) {
            console.error("Config save failure:", resp);
            alert("Failed saving config: " + resp.status);
            return;
          }
          const result = await resp.json();
          if (result.err) {
            console.error("Failed saving config:", result);
            alert(result.err);
            return;
          }
          ta.readOnly = true;
          stopEditTextArea(ta);
          for (let btn of cfe.querySelectorAll("button")) {
            btn.disabled = true;
          }
        } catch (err) {
          console.error("Error saving config:", err);
          alert("Failed saving config: " + err);
        }
      }
      break;
    case "cancel":
      for (let ta of cfe.querySelectorAll("textarea")) {
        ta.readOnly = true;
        ta.value = ta.dataset.preEdit;
        stopEditTextArea(ta);
      }
      for (let btn of cfe.querySelectorAll("button")) {
        btn.disabled = true;
      }
      break;
    default:
      console.warn("Unknown button action:", btn.dataset.act, btn);
  }
});
