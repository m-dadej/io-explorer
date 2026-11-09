// =============================================================================
// Charts (Observable Plot), same style and colours as the R app (app/charts.R).
// Every chart has hover tooltips. Each function takes the width and returns a node.
// =============================================================================

import { fmt } from './ui.js';

export const ink = '#0b0b0b', ink2 = '#52514e', axisCol = '#c3c2b7', blue = '#2a78d6', rest = '#c9c7bf';
export const hhGreen = '#1baf7a';

// Fixed colour per final demand category (government stands out in orange)
export const categoryColours = { P3_S14: '#2a78d6', P3_S13: '#eb6834', P3_S15: '#1baf7a', P51G: '#eda100', P5M: '#e87ba4' };
// Parts of a shock's effect: supply-chain rounds in blues (darker = closer to the shock), household spending green
export const effectColours = [['Direct', '#1a4f9c'], ['First-round suppliers', '#3a80d8'],
  ['Further supply-chain rounds', '#7fb3ee'], ['Household spending', hhGreen]];
export const contentColours = [['Home GVA', blue], ['GVA abroad', '#eb6834'], ['Taxes on products & other', rest]];
export const multiplierColours = [['The industry itself', '#184f95'], ['Other home industries', '#7fb3ee'],
  ['GVA abroad', '#eb6834'], ['Taxes on products & other', rest]];
export const hhLabel = 'From household spending';
// Parts of a price shock's cost push, and the two price measures by country (grey, blue)
export const priceColours = [['Direct purchases', '#1a4f9c'], ['Through other suppliers', '#7fb3ee']];
export const priceMeasures = ['Output prices', 'Household basket'];
// Value added in exports: home blue, foreign orange, taxes and other grey
export const exportColours = [['Home GVA', blue], ['Foreign GVA', '#eb6834'], ['Taxes on products & other', rest]];
// Trade shock: the change in GVA through sales of the country's own producers or of producers abroad
export const channelColours = [['Own country’s sales', '#1a4f9c'], ['Other countries’ sales', '#7fb3ee'], ['Household spending', hhGreen]];
// Industry profile: what each EUR of output pays for; buyers; home vs abroad
export const costColours = [['Compensation of employees', '#184f95'], ['Gross operating surplus', '#3a80d8'],
  ['Other taxes on production', '#a9cbf2'], ['Inputs from home industries', '#8f8d86'], ['Imported inputs', '#eb6834'],
  ['Taxes on products & other', '#d9d7cf']];
export const buyerColours = [['Industries (inputs)', '#184f95'], ['Final buyers', '#7fb3ee']];
export const homeColours = [blue, '#eb6834'];

const ctx = document.createElement('canvas').getContext('2d');
ctx.font = '12px system-ui, -apple-system, "Segoe UI", Roboto, sans-serif';
function marginFor(labels, max = 300) {
  const w = Math.max(0, ...labels.map(l => ctx.measureText(String(l)).width));
  return Math.min(max, Math.ceil(w) + 12);
}
// Left margin for the row labels and a label format that shortens labels too long for it
// (on phones the margin is narrower; the tooltip still has the full label)
function rowLabels(labels, width) {
  const marginLeft = marginFor(labels, width < 600 ? 150 : 300);
  const fit = l => ctx.measureText(l).width <= marginLeft - 12;
  const tickFormat = l => {
    let s = String(l);
    if (fit(s)) return s;
    while (s.length > 1 && !fit(`${s}…`)) s = s.slice(0, -1);
    return `${s}…`;
  };
  return { marginLeft, tickFormat };
}
const style = { fontSize: '12px', fontFamily: 'inherit', color: ink2, background: 'transparent', overflow: 'visible' };
const valueText = (v, percent) => percent ? fmt.pct(v, 2) : `${fmt.num(v, 1)}`;
const plainNumber = new Intl.NumberFormat('en-GB', { maximumFractionDigits: 2 });
const tickFormat = percent => percent ? '~%' : x => plainNumber.format(x);

// Horizontal bars, first row on top. highlight(d) -> true paints the bar blue, others grey (or highlightColours).
export function barsH(width, { rows, label, value, percent = true, xTitle, caption, highlight = null,
                               highlightNames = ['Highlighted', 'Other'], highlightColours = [blue, rest], tip = null }) {
  const labels = rows.map(label);
  const fill = highlight ? d => (highlight(d) ? highlightNames[0] : highlightNames[1]) : blue;
  return Plot.plot({
    width, height: 46 + 24 * rows.length + (highlight ? 26 : 0), marginLeft: rowLabels(labels, width).marginLeft, marginRight: 24, marginTop: 34,
    style, caption,
    x: { axis: 'top', label: xTitle, labelAnchor: 'left', grid: true, nice: true, tickFormat: tickFormat(percent) },
    y: { domain: labels, label: null, tickSize: 0, tickFormat: rowLabels(labels, width).tickFormat },
    color: highlight ? { domain: highlightNames, range: highlightColours, legend: true } : undefined,
    marks: [
      Plot.barX(rows, { x: value, y: label, fill, insetTop: 3, insetBottom: 3, tip: true,
                        title: d => tip ? tip(d) : `${label(d)}\n${valueText(value(d), percent)}` }),
      Plot.ruleX([0], { stroke: axisCol })
    ]
  });
}

// Horizontal stacked bars. rows: {label, category, value}; colours: [[category, colour], ...] in stacking order.
// whole: the parts of each bar add up to 100% (the axis then ends at 100%, not at a rounded 110%).
export function stackedH(width, { rows, colours, percent = true, xTitle, caption, labels = null, whole = false }) {
  const lab = labels ?? [...new Set(rows.map(r => r.label))];
  const used = colours.filter(([c]) => rows.some(r => r.category === c));
  const domain = used.map(c => c[0]);
  // Tiny negative parts (e.g. a small fall in inventories) would stretch the axis below zero: start it at 0
  const pos = new Map(), neg = new Map();
  rows.forEach(r => { const m = r.value < 0 ? neg : pos; m.set(r.label, (m.get(r.label) ?? 0) + r.value); });
  const maxPos = Math.max(0, ...pos.values()), minNeg = Math.min(0, ...neg.values());
  let xDomain = minNeg < 0 && -minNeg < 0.01 * maxPos ? [0, maxPos] : undefined;
  if (whole && maxPos <= 1.02) xDomain = [xDomain ? 0 : minNeg, 1];
  return Plot.plot({
    width, height: 72 + 26 * lab.length, marginLeft: rowLabels(lab, width).marginLeft, marginRight: 24, marginTop: 34,
    style, caption,
    x: { axis: 'top', label: xTitle, labelAnchor: 'left', grid: true, nice: true, tickFormat: tickFormat(percent), domain: xDomain, clamp: !!xDomain },
    y: { domain: lab, label: null, tickSize: 0, tickFormat: rowLabels(lab, width).tickFormat },
    color: { domain, range: used.map(c => c[1]), legend: true },
    marks: [
      Plot.barX(rows, { x: 'value', y: 'label', fill: 'category', order: domain, insetTop: 3, insetBottom: 3,
                        stroke: '#fff', strokeWidth: 1, tip: true,
                        title: d => `${d.label}\n${d.category}: ${valueText(d.value, percent)}` }),
      Plot.ruleX([0], { stroke: axisCol })
    ]
  });
}

// Two measures side by side per row (first grey, second blue)
export function pairedH(width, { rows, measures, labels, xTitle, caption }) {
  return Plot.plot({
    width, height: 70 + 34 * labels.length, marginLeft: rowLabels(labels, width).marginLeft, marginRight: 24, marginTop: 34,
    style, caption,
    x: { axis: 'top', label: xTitle, labelAnchor: 'left', grid: true, nice: true, tickFormat: '~%' },
    y: { axis: null, domain: measures },
    fy: { domain: labels, label: null, axis: 'left', tickSize: 0, padding: 0.18, tickFormat: rowLabels(labels, width).tickFormat },
    color: { domain: measures, range: [rest, blue], legend: true },
    marks: [
      Plot.barX(rows, { x: 'value', y: 'measure', fy: 'label', fill: 'measure', tip: true,
                        title: d => `${d.label}\n${d.measure}: ${fmt.pct(d.value, 2)}` }),
      Plot.ruleX([0], { stroke: axisCol })
    ]
  });
}

// Heatmap: a coloured table. rows: {row, col, value} (shares). The colour shows the size of the number,
// the label its sign. Columns in plainCols show their numbers without colour.
export function heatmap(width, { rows, rowLevels, colLevels, caption, digits = 1, plainCols = [] }) {
  const plain = new Set(plainCols);
  const sizes = rows.filter(d => !plain.has(d.col)).map(d => Math.abs(d.value));
  const max = Math.max(1e-12, ...sizes);
  const scale = d3.scaleLinear([0, max], ['#eef5fd', '#184f95']);
  const { marginLeft, tickFormat } = rowLabels(rowLevels, width);
  width = Math.max(width, marginLeft + 46 * colLevels.length + 10);   // narrower than this: the chart scrolls sideways
  const colWidth = (width - marginLeft - 10) / colLevels.length;
  const rotate = colWidth < 8 * Math.max(...colLevels.map(c => String(c).length));
  return Plot.plot({
    width, height: 30 * rowLevels.length + (rotate ? 90 : 46), marginLeft, marginRight: 10, marginTop: rotate ? 80 : 36,
    style, caption,
    x: { axis: 'top', domain: colLevels, label: null, tickSize: 0, tickRotate: rotate ? -35 : 0 },
    y: { domain: rowLevels, label: null, tickSize: 0, tickFormat },
    color: { type: 'identity' },
    marks: [
      Plot.cell(rows, { x: 'col', y: 'row', inset: 1, fill: d => plain.has(d.col) ? '#f4f3ef' : scale(Math.abs(d.value)),
                        tip: true, title: d => `${d.row}\n${d.col}: ${fmt.pct(d.value, digits + 1)}` }),
      Plot.text(rows, { x: 'col', y: 'row', fontSize: 11,
                        text: d => fmt.num(Math.round(100 * d.value * 10 ** digits) / 10 ** digits + 0, digits),
                        fill: d => !plain.has(d.col) && Math.abs(d.value) > 0.55 * max ? '#ffffff' : ink })
    ]
  });
}
