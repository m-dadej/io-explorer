// =============================================================================
// Readings of the results in words: the "In plain words" boxes. One function per chart; each takes
// the numbers the chart shows and returns a sentence or two ('' when there is nothing to say).
// The same texts as in the R app (the `read` functions in app/app.R). No DOM code, so it runs in
// Node for tests.
// =============================================================================

import { fmt } from './ui.js';
import { sum } from './engine.js';

const per100 = x => `EUR ${fmt.num(100 * x, 1)}`;   // a share as EUR per EUR 100
const andList = a => a.length < 2 ? a.join('') : `${a.slice(0, -1).join(', ')} and ${a.at(-1)}`;
const maxBy = (a, f) => a.reduce((best, r) => (best === null || f(r) > f(best) ? r : best), null);
const minBy = (a, f) => maxBy(a, r => -f(r));
const shortCategory = x => x.replace(/ \(.*\)$/, '');   // "Investment (GFCF)" -> "Investment"
const ppText = x => Math.abs(x) < 0.0005 ? 'less than 0.001' : fmt.num(Math.abs(x), 3);

export function makeReadings(model, year) {
  const allSectors = s => s.length === 1 && s[0] === 'All sectors';
  const describeSectors = s => allSectors(s) ? 'all industries' : s.map(x => model.sectorNames[x] ?? x).join(' + ');
  // "Motor vehicles in Germany", or "Germany" for all industries
  const describeIn = (c, s) => allSectors(s) ? model.describe(c) : `${describeSectors(s)} in ${model.describe(c)}`;
  const productionOf = (c, s) => allSectors(s) ? `all production in ${model.describe(c)}` : `production of ${describeIn(c, s)}`;
  const productsOf = (c, s) => allSectors(s) ? `products made in ${model.describe(c)}` : `products of ${describeIn(c, s)}`;

  return {
    // ---- 1. Where VA ends up ----
    // foreign: foreign buyers sorted by total share over the chosen categories
    destTop({ c, s, catLabels, allCats, home, abroad, foreign }) {
      let t = `Of every EUR 100 of value added created by ${describeIn(c, s)} in ${year}, ${per100(home)} is finally used at home and ${per100(abroad)} abroad.`;
      const top = foreign.filter(b => b.total > 0).slice(0, 3);
      if (top.length) {
        const only = allCats ? '' : ` (counting only ${catLabels.join(', ')})`;
        t += ` The largest foreign buyers${only} are ${andList(top.map(b => `${b.buyer_name} (${per100(b.total)})`))}.`;
      }
      return t;
    },
    // The buyers with the largest gap between the two shares, in each direction
    destExports(ex) {
      const big = ex.filter(r => Math.max(r.exports_share, r.va_share) >= 0.01);
      if (!big.length) return '';
      const u = maxBy(big, r => r.exports_share - r.va_share), o = maxBy(big, r => r.va_share - r.exports_share);
      const out = [];
      if (u.exports_share - u.va_share > 0.005) {
        out.push(`${u.buyer_name}: ${fmt.pct(u.exports_share)} of direct exports, but only ${fmt.pct(u.va_share)} of the value added used abroad. Part of what is shipped there is sold on to other countries, or was made with imported parts.`);
      }
      if (o.va_share - o.exports_share > 0.005) {
        out.push(`${o.buyer_name}: ${fmt.pct(o.va_share)} of the value added used abroad, but only ${fmt.pct(o.exports_share)} of direct exports. Much of it arrives inside other countries' products.`);
      }
      return out.join(' ');
    },
    // long: {row, col, value}; col "Home" or a country code
    destHeat(long) {
      const colName = code => code === 'RoW' ? 'the rest of the world' : model.countryNames[code] ?? code;
      const fr = long.filter(r => r.col !== 'Home'), hm = long.filter(r => r.col === 'Home');
      const b = maxBy(fr, r => r.value), l = hm.length > 1 ? minBy(hm, r => r.value) : null;
      const out = [];
      if (b && b.value > 0) {
        out.push(`Read along a row. Example: of every EUR 100 of value added in ${b.row}, ${per100(b.value)} is used by final buyers in ${colName(b.col)}, the largest foreign cell here.`);
      }
      if (l) out.push(`${l.row} depends least on home buyers: only ${per100(l.value)} of every EUR 100 is used at home.`);
      return out.join(' ');
    },

    // ---- 2. Exposure to a source ----
    expo({ type, srcC, srcS, show, dep, va, res }) {
      const sh = dep / va;
      let t = type === 'production'
        ? `If ${productionOf(srcC, srcS)} stopped altogether, the countries shown (${model.describe(show)}) would lose ${fmt.amount(dep)} of value added, ${fmt.pct(sh, 2)} of their GVA. A 10% fall in its output would take away a tenth of this: ${fmt.amount(dep / 10)}.`
        : `${fmt.amount(dep)} of value added in the countries shown (${model.describe(show)}), ${fmt.pct(sh, 2)} of their GVA, depends on final demand anywhere in the world for ${productsOf(srcC, srcS)}. If that demand fell by 10%, they would lose about ${fmt.amount(dep / 10)}.`;
      const top = res.filter(r => r.share > 0).slice(0, 3);
      if (top.length) t += ` Most exposed: ${andList(top.map(r => `${r.name} (${fmt.pct(r.share)} of its own GVA)`))}.`;
      return t;
    },

    // ---- 3. Sources of an origin ----
    src({ c, s, type, level, res }) {
      const top = res.slice(0, 3);
      if (!top.length) return '';
      const [a, ...rest] = top, origin = describeIn(c, s);
      const next = rest.length ? ` Next: ${andList(rest.map(r => `${r.source_name} (${fmt.pct(r.share, 2)})`))}.` : '';
      return type === 'production'
        ? `${fmt.pct(a.share, 2)} of the value added of ${origin} depends on production ${level === 'country' ? 'in' : 'of'} ${a.source_name}: that much would be lost if it stopped.${next} These shares overlap (supply chains share suppliers), so do not add them up.`
        : `${fmt.pct(a.share, 2)} of the value added of ${origin} ends up in final products ${level === 'country' ? 'made in' : 'of'} ${a.source_name}, wherever they are bought.${next} These shares can be added: together, the ${fmt.num(res.length)} sources in the table account for ${fmt.pct(sum(res.map(r => r.share)))}.`;
    },

    // ---- 4. Top pairs ----
    pairs({ type, res }) {
      const p = res[0];
      if (!p) return '';
      let t = `Tightest link: ${fmt.pct(p.share)} of the value added of ${p.origin} (${fmt.amount(p.va_dependent)}) depends on ${type === 'production' ? 'production of' : 'final demand for products of'} ${p.source}.`;
      if (p.share >= 0.15) t += ' Shares this high usually involve a small industry, or one that sells most of its output to one customer: check the EUR columns in the table.';
      return t;
    },

    // ---- 5. Shock simulator ----
    simHeadline({ show, shown, world }) {
      if (!shown.dva) return '';
      const verb = x => x < 0 ? 'falls' : 'rises', neg = shown.dva < 0;
      return `Value added in the countries shown (${model.describe(show)}) ${verb(shown.dva)} by ${fmt.amount(Math.abs(shown.dva))}, ${fmt.pct(Math.abs(shown.share_va), 3)} of their GVA. As a rough guide, that ${neg ? 'takes' : 'adds'} ${ppText(100 * shown.share_va)} percentage points ${neg ? 'off' : 'to'} their GDP growth in the year it happens. Worldwide, value added ${verb(world.dva)} by ${fmt.amount(Math.abs(world.dva))}.`;
    },
    // rows: the countries in the chart, most affected first
    simCountries({ rows, scope, exclShocked }) {
      const top = rows.slice(0, 3);
      if (!top.length) return '';
      const where = scope === 'shown' ? '' : exclShocked ? ' outside the countries where the shock happens' : ' worldwide';
      return `Most affected${where}: ${andList(top.map(r => `${r.country_name} (${fmt.pct(r.share_va, 3)} of its GVA)`))}.`;
    },
    simCs({ top, shown, induced }) {
      if (!top) return '';
      let t = `Most affected country-industry: ${top.label}, ${fmt.pct(top.share_va, 2)} of its own GVA (${fmt.amount(top.dva)}).`;
      if (shown.dva) {
        t += ` Of the change in the countries shown, ${fmt.pct((shown.dva_first + shown.dva_further) / shown.dva, 0)} comes through suppliers (first round and further)${induced ? ` and ${fmt.pct(shown.dva_induced / shown.dva, 0)} through household spending` : ''}.`;
      }
      return t;
    },
    // long: {row: industry group, col: country code, value}
    simHeat(long) {
      const b = maxBy(long.filter(r => Number.isFinite(r.value)), r => Math.abs(r.value));
      return b ? `Read down a column for one country. The largest change relative to size: ${b.row} in ${model.countryNames[b.col]}, ${fmt.pct(b.value, 2)} of the group's GVA.` : '';
    },

    // ---- 6. Domestic shock ----
    // h: homeVsAbroad(); abroad: other countries, largest change first
    domHeadline({ c, fd, induced, size, prodS, h, abroad }) {
      if (!Number.isFinite(h.home_per_eur)) return '';
      const cname = model.countryNames[c];
      let t;
      if (fd && induced) {
        t = `Of each EUR 1 of this shock, ${fmt.eur(h.home_per_eur_type1)} becomes value added in ${cname} through the supply chain, and ${fmt.eur(h.home_per_eur)} in all once households adjust their spending to the change in wages. ${fmt.eur(h.abroad_per_eur)} becomes value added abroad.`;
      } else if (fd) {
        t = `Of each EUR 1 of this shock, ${fmt.eur(h.home_per_eur)} becomes value added in ${cname} and ${fmt.eur(h.abroad_per_eur)} value added abroad (imports, including the imported parts of home-made products). The remaining ${fmt.eur(1 - h.home_per_eur - h.abroad_per_eur)} goes on taxes on products along the supply chain.`;
      } else {
        t = `Each EUR 1 of ${size < 0 ? 'lost' : 'extra'} output of ${describeSectors(prodS)} ${size < 0 ? 'takes away' : 'brings'} ${fmt.eur(h.home_per_eur)} of value added in ${cname} (${fmt.eur((h.dva_home - h.dva_home_other) / h.shock)} in the industries themselves and ${fmt.eur(h.dva_home_other / h.shock)} in other home industries, ${induced ? 'household spending included' : 'their home suppliers'}) and ${fmt.eur(h.abroad_per_eur)} abroad, through imported inputs.`;
      }
      return `${t} In all, ${fmt.pct(h.leakage)} of the change in value added happens abroad${abroad.length ? `; the largest part in ${abroad[0].country_name} (${fmt.amount(Math.abs(abroad[0].dva))}).` : '.'}`;
    },
    // rows: home industries (or groups), most affected first, with name, change (EUR m) and share
    domInd({ rows, pct, prod, loss }) {
      const top = rows.slice(0, 3);
      if (!top.length) return '';
      const val = r => pct ? `${fmt.pct(Math.abs(r.share), 2)} of its GVA` : fmt.amount(Math.abs(r.change));
      return `Industries that ${loss ? 'lose' : 'gain'} most${pct ? ' relative to their size' : ''}${prod ? ' (outside the shocked industries)' : ''}: ${andList(top.map(r => `${r.name} (${val(r)})`))}.`;
    },
    // e: homeDemandExposure() rows; names: industries by GVA, largest first
    domExpo({ e, names }) {
      const f = names[0];
      if (!f) return '';
      const shr = (nm, k) => sum(e.filter(r => r.origin_name === nm && r.code === k).map(r => r.share));
      const fr = maxBy(e.filter(r => r.code === 'FOREIGN' && names.includes(r.origin_name)), r => r.share);
      return `Example: of every EUR 100 of value added in ${f}, ${per100(shr(f, 'P3_S14'))} depends on home households, ${per100(shr(f, 'P3_S13'))} on the home government, ${per100(shr(f, 'P51G'))} on home investment and ${per100(shr(f, 'FOREIGN'))} on buyers abroad. Most dependent on buyers abroad: ${fr.origin_name} (${fmt.pct(fr.share)}).`;
    },
    // rows: domesticContent()
    domContent({ c, rows, induced }) {
      const cats = rows.filter(r => r.code !== 'P5M' && r.code !== 'ALL' && r.final_demand > 0);
      const hi = maxBy(cats, r => r.home_per_eur), lo = minBy(cats, r => r.home_per_eur), all = rows.find(r => r.code === 'ALL');
      if (!hi || !all) return '';
      let t = `On average, ${fmt.eur(all.home_per_eur)} of each EUR 1 that buyers in ${model.countryNames[c]} spend becomes home value added. The most stays at home for ${shortCategory(hi.category)} (${fmt.eur(hi.home_per_eur)}), the least for ${shortCategory(lo.category)} (${fmt.eur(lo.home_per_eur)}): the more a kind of spending relies on imports, the less stays at home.`;
      if (induced) t += ` Household spending of the extra wages adds ${fmt.eur(all.home_hh_per_eur)} on average (the green part).`;
      return t;
    },
    // rows: domesticMultipliers() with total_home
    domMult({ c, rows }) {
      // Households as employers (T) buy no inputs, so all of their euro stays at home: not a useful example
      const ok = rows.filter(r => r.sector !== 'T' && Number.isFinite(r.total_home));
      if (ok.length < 2) return '';
      const hi = maxBy(ok, r => r.total_home), lo = minBy(ok, r => r.total_home);
      return `Of each EUR 1 that buyers anywhere spend on products of ${hi.sector_name}, ${fmt.eur(hi.total_home)} stays in ${model.countryNames[c]} as value added; for ${lo.sector_name}, only ${fmt.eur(lo.total_home)}. Industries that use many imported inputs keep less at home.`;
    },

    // ---- 7. Government ----
    // rows: demandShares(), most dependent first
    govShare(rows) {
      if (!rows.length) return '';
      if (rows.length === 1) {
        const r = rows[0];
        return `In ${r.country_name}, ${fmt.pct(r.share_domestic)} of value added depends on consumption of its own government, and ${fmt.pct(r.share_foreign)} on that of foreign governments.`;
      }
      const hi = rows[0], lo = rows.at(-1);
      return `Most dependent: ${hi.country_name}, with ${fmt.pct(hi.share_domestic)} of its value added used by its own government's consumption and ${fmt.pct(hi.share_foreign)} by foreign governments'. Least dependent: ${lo.country_name}, with ${fmt.pct(lo.share_domestic)} and ${fmt.pct(lo.share_foreign)}.`;
    },
    govBenef({ gov, total, top, byShare }) {
      if (!top) return '';
      return `Consumption of the government of ${model.countryNames[gov]} supports ${fmt.amount(total.dva)} of value added in the countries shown, ${fmt.pct(total.share_va, 2)} of their GVA. ${byShare ? 'Most dependent' : 'Largest amount'}: ${top.name} (${fmt.pct(top.share_va, 2)} of its own GVA, ${fmt.amount(top.dva)}).`;
    }
  };
}
