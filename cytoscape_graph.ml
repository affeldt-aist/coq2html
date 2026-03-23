let render_compound_graph ~id_prefix ~title ~hint ~elements_json =
  let template = {|
<style>
  #__ID__-section {
    position: relative;
    background: #fff;
    color: #222;
  }
  .__ID__-toolbar {
    display: flex;
    gap: 6px;
    margin: 0.5em 0 0.4em;
    flex-wrap: wrap;
    align-items: center;
  }
  .__ID__-toolbar button {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 5px 11px;
    font-size: 13px;
    cursor: pointer;
    border: 1px solid #bbb;
    border-radius: 4px;
    background: #f5f5f5;
    color: #333;
    user-select: none;
    white-space: nowrap;
  }
  .__ID__-toolbar button:hover { background: #e2e2e2; }
  .darkmode--activated #__ID__-section {
    background: #171717;
    color: #ececec;
  }
  .darkmode--activated .__ID__-toolbar button {
    border-color: #5d5d5d;
    background: #2b2b2b;
    color: #f2f2f2;
  }
  .darkmode--activated .__ID__-toolbar button:hover {
    background: #393939;
  }
  .__ID__-toolbar .sep {
    color: #bbb;
    padding: 0 2px;
    user-select: none;
  }
  .darkmode--activated .__ID__-toolbar .sep { color: #888; }
  .__ID__-hint {
    font-size: 12px;
    color: #888;
    margin: 0 0 0.4em;
  }
  .darkmode--activated .__ID__-hint { color: #b7b7b7; }
  #__ID__-cytoscape {
    width: 100%;
    height: 70vh;
    min-height: 300px;
    border: 1px solid #d7d7d7;
    margin: 0 0 1em;
  }
  .darkmode--activated #__ID__-cytoscape {
    border-color: #4f4f4f;
  }
</style>
<div id="__ID__-section">
<h2>__TITLE__</h2>
<div class="__ID__-toolbar">
  <button type="button" id="__ID__-btn-zoom-in"      title="Zoom in">&#xFF0B; Zoom in</button>
  <button type="button" id="__ID__-btn-zoom-out"     title="Zoom out">&#xFF0D; Zoom out</button>
  <button type="button" id="__ID__-btn-fit"          title="Fit whole graph in view">&#x229E; Fit</button>
  <span class="sep">|</span>
  <button type="button" id="__ID__-btn-collapse-all" title="Collapse all cluster groups">&#x229F; Collapse groups</button>
  <button type="button" id="__ID__-btn-expand-all"   title="Expand all cluster groups">&#x229E; Expand groups</button>
  <span class="sep">|</span>
  <button type="button" id="__ID__-btn-fs"           title="Toggle full-screen">&#x26F6; Full screen</button>
</div>
<p class="__ID__-hint">__HINT__</p>
<div id="__ID__-cytoscape"></div>
</div>
<script src="https://unpkg.com/cytoscape@latest/dist/cytoscape.min.js"></script>
<script src="https://unpkg.com/dagre@0.8.5/dist/dagre.min.js"></script>
<script src="https://unpkg.com/cytoscape-dagre@2.5.0/cytoscape-dagre.js"></script>
<script>
(function () {
  var dagreLayout = {
    name: 'dagre',
    nodeSep: 20,
    edgeSep: 10,
    rankSep: 50,
    padding: 20,
    fit: false,
    animate: false
  };

  var styleForTheme = function (isDark) {
    var palette = isDark
      ? {
          sectionBg: '#171717',
          nodeBg: '#5a8fc7',
          nodeText: '#f7f7f7',
          parentBg: '#7b6a59',
          parentBorder: '#c8b7a8',
          parentText: '#f3ece7',
          plusBg: '#73b873',
          edge: '#b7b7b7',
          selectedBg: '#ff8d72',
          selectedBorder: '#ffd0c2'
        }
      : {
          sectionBg: '#ffffff',
          nodeBg: '#2f6f9f',
          nodeText: '#ffffff',
          parentBg: '#f4efe9',
          parentBorder: '#c9b6a9',
          parentText: '#4a3b32',
          plusBg: '#5a8a5a',
          edge: '#8b8b8b',
          selectedBg: '#b85042',
          selectedBorder: '#5e201a'
        }
    ;
    return [
      {
        selector: '.hidden',
        style: { 'display': 'none' }
      },
      {
        selector: 'node',
        style: {
          'label': 'data(name)',
          'background-color': palette.nodeBg,
          'color': palette.nodeText,
          'text-wrap': 'wrap',
          'text-max-width': 180,
          'text-valign': 'center',
          'text-halign': 'center',
          'font-size': 11,
          'shape': 'round-rectangle',
          'width': 'label',
          'height': 'label',
          'padding': '10px',
          'border-width': 1,
          'border-color': 'rgba(0, 0, 0, 0.12)'
        }
      },
      {
        selector: ':parent',
        style: {
          'label': 'data(name)',
          'background-color': palette.parentBg,
          'background-opacity': isDark ? 0.22 : 0.35,
          'border-color': palette.parentBorder,
          'border-width': 2,
          'color': palette.parentText,
          'text-valign': 'top',
          'text-halign': 'center',
          'text-wrap': 'wrap',
          'text-max-width': 220,
          'font-size': 12,
          'padding': '18px',
          'min-width': 'label',
          'min-width-bias-left': '50%',
          'min-width-bias-right': '50%',
          'min-height': 'label',
          'min-height-bias-top': '100%',
          'min-height-bias-bottom': '0%',
          'cursor': 'pointer'
        }
      },
      {
        selector: 'node[name="+"]',
        style: {
          'background-color': palette.plusBg,
          'color': '#ffffff',
          'font-size': 16,
          'font-weight': 'bold',
          'width': 26,
          'height': 26,
          'shape': 'ellipse',
          'padding': 0,
          'cursor': 'pointer'
        }
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'bezier',
          'width': 2,
          'line-color': palette.edge,
          'target-arrow-color': palette.edge,
          'target-arrow-shape': 'triangle'
        }
      },
      {
        selector: 'node:selected',
        style: {
          'background-color': palette.selectedBg,
          'border-width': 3,
          'border-color': palette.selectedBorder
        }
      }
    ];
  };

  var isDarkMode = function () {
    return document.body.classList.contains('darkmode--activated')
      || document.documentElement.classList.contains('darkmode--activated');
  };

  var boot = function () {
    var section   = document.getElementById("__ID__-section");
    var container = document.getElementById("__ID__-cytoscape");
    if (!section || !container || typeof cytoscape === "undefined") return;

    var cy = cytoscape({
      container: container,
      elements: __ELEMENTS__,
      boxSelectionEnabled: false,
      autoungrabify: false,
      wheelSensitivity: 0.15,
      style: styleForTheme(isDarkMode()),
      layout: dagreLayout
    });

    var applyTheme = function (refit) {
      var dark = isDarkMode();
      section.style.backgroundColor = dark ? '#171717' : '#ffffff';
      cy.style(styleForTheme(dark));
      cy.resize();
      if (refit) cy.fit(undefined, 30);
    };

    cy.nodes().forEach(function (n) {
      if (n.data('name') === '+') {
        n.addClass('hidden');
        n.relativePosition({ x: 0, y: 0 });
      }
    });
    cy.fit(undefined, 30);

    var collapseToggle = function (parent) {
      parent.children().forEach(function (child) {
        child.toggleClass('hidden');
        if (child.data('name') === '+' && !child.hasClass('hidden')) {
          child.relativePosition({ x: 0, y: 0 });
        }
      });
    };

    var collapseAll = function () {
      cy.nodes().forEach(function (n) {
        if (n.isParent()) {
          n.children().forEach(function (child) {
            if (child.data('name') === '+') {
              child.removeClass('hidden');
              child.relativePosition({ x: 0, y: 0 });
            } else {
              child.addClass('hidden');
            }
          });
        }
      });
      cy.layout(dagreLayout).run();
      cy.fit(undefined, 30);
    };

    var expandAll = function () {
      cy.nodes().forEach(function (n) {
        if (n.data('name') === '+') {
          n.addClass('hidden');
          n.relativePosition({ x: 0, y: 0 });
        } else {
          n.removeClass('hidden');
        }
      });
      cy.layout(dagreLayout).run();
      cy.fit(undefined, 30);
    };

    var fsBtn   = document.getElementById("__ID__-btn-fs");
    var toolbar = section.querySelector(".__ID__-toolbar");
    var heading = section.querySelector("h2");
    var hint    = section.querySelector(".__ID__-hint");

    var dimensionInPixels = function (value) {
      var n = parseFloat(value);
      return Number.isFinite(n) ? n : 0;
    };
    var elementOuterHeight = function (elt) {
      if (!elt) return 0;
      var style = window.getComputedStyle(elt);
      var margins = dimensionInPixels(style.marginTop) + dimensionInPixels(style.marginBottom);
      return elt.getBoundingClientRect().height + margins;
    };
    var updateViewportSize = function (refit) {
      var inFs = (document.fullscreenElement === section) || (document.webkitFullscreenElement === section);
      if (inFs) {
        var available = window.innerHeight
          - elementOuterHeight(heading)
          - elementOuterHeight(toolbar)
          - elementOuterHeight(hint);
        container.style.height = Math.max(220, Math.floor(available)) + "px";
      } else {
        container.style.height = "";
      }
      cy.resize();
      if (refit) cy.fit(undefined, 30);
    };

    document.getElementById("__ID__-btn-zoom-in").addEventListener("click", function () {
      cy.zoom({ level: cy.zoom() * 1.3, renderedPosition: { x: container.offsetWidth / 2, y: container.offsetHeight / 2 } });
    });
    document.getElementById("__ID__-btn-zoom-out").addEventListener("click", function () {
      cy.zoom({ level: cy.zoom() / 1.3, renderedPosition: { x: container.offsetWidth / 2, y: container.offsetHeight / 2 } });
    });
    document.getElementById("__ID__-btn-fit").addEventListener("click", function () {
      cy.fit(undefined, 30);
    });
    document.getElementById("__ID__-btn-collapse-all").addEventListener("click", collapseAll);
    document.getElementById("__ID__-btn-expand-all").addEventListener("click", expandAll);

    fsBtn.addEventListener("click", function () {
      if (!document.fullscreenElement && !document.webkitFullscreenElement) {
        (section.requestFullscreen || section.webkitRequestFullscreen).call(section);
      } else {
        (document.exitFullscreen || document.webkitExitFullscreen).call(document);
      }
    });
    var onFsChange = function () {
      var inFs = !!(document.fullscreenElement || document.webkitFullscreenElement);
      fsBtn.innerHTML = inFs ? "&#x2715; Exit full screen" : "&#x26F6; Full screen";
      updateViewportSize(true);
    };
    document.addEventListener("fullscreenchange",       onFsChange);
    document.addEventListener("webkitfullscreenchange", onFsChange);
    window.addEventListener("resize", function () { updateViewportSize(false); });

    var darkModeTarget = document.body || document.documentElement;
    if (typeof MutationObserver !== 'undefined' && darkModeTarget) {
      var darkModeObserver = new MutationObserver(function () {
        applyTheme(false);
      });
      darkModeObserver.observe(darkModeTarget, {
        attributes: true,
        attributeFilter: ['class']
      });
    }

    cy.on('tap', 'node', function (evt) {
      var node = evt.target;
      if (node.data('name') === '+') {
        collapseToggle(node.parent());
      } else if (node.isParent()) {
        collapseToggle(node);
      } else {
        var url = node.data('url');
        if (url) window.location.href = url;
      }
    });

    applyTheme(false);
    updateViewportSize(true);
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot, { once: true });
  } else {
    boot();
  }
})();
</script>
|}
  in
  let replace token value txt =
    Str.global_replace (Str.regexp_string token) value txt
  in
  template
  |> replace "__ID__" id_prefix
  |> replace "__TITLE__" title
  |> replace "__HINT__" hint
  |> replace "__ELEMENTS__" elements_json
