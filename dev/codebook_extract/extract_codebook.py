"""
Extract the frequency tables from a NEPS codebook PDF (FDZ-LIfBi, CC BY 4.0)
into two CSV files for NEPScribe. Works for the German and the English codebooks.

  codebook_freq_<SC>_<version>.csv   one row per value of a variable:
      dataset, variable, row, value, label, missing, n_total, w1 ... w17
      (w1 ... are empty if the codebook gives no breakdown by wave;
       the number of wave columns follows the codebook)
  codebook_vars_<SC>_<version>.csv   one row per variable:
      dataset, variable, varlabel, has_table, by_wave, n_rows, omitted_values

Usage:  python extract_codebook.py NEPS_SC6_Codebook_17-0-0_de.pdf SC6 17-0-0 [outdir] [suffix]
        (suffix, e.g. de or en, is appended to the file names)
"""
import csv, re, sys, os
import pdfplumber

NUM = re.compile(r'^-?\d+(\.\d+)?$')
VARNAME = re.compile(r'^[A-Za-z][A-Za-z0-9_]*$')
LEADERS = re.compile(r'(\. ){3,}')   # dot leaders in the tables of contents

def norm(s):
    return s.replace('‐', '-').replace('−', '-').replace('‑', '-')

def lines_of(words, tol=2.0):
    """group words into lines by their top coordinate"""
    lines = []
    for w in sorted(words, key=lambda w: (round(w['top']), w['x0'])):
        if lines and abs(lines[-1][0]['top'] - w['top']) <= tol:
            lines[-1].append(w)
        else:
            lines.append([w])
    return [sorted(l, key=lambda w: w['x0']) for l in lines]

def split_overprinted(w, chars, x_split):
    """A long label without spaces can run into the number columns; the PDF then
    prints label and numbers over each other and pdfplumber merges them into one
    word. Separate the digits right of x_split (the numbers are right-aligned in
    their columns) from the label characters."""
    cs = sorted((c for c in chars if abs(c['top'] - w['top']) < 2
                 and c['x0'] >= w['x0'] - 0.5 and c['x1'] <= w['x1'] + 0.5), key=lambda c: c['x0'])
    label, nums = [], []
    for c in cs:
        (nums if c['x0'] >= x_split and (c['text'].isdigit() or c['text'] in '-.‐−‑') else label).append(c)
    if not nums:
        return [w]
    out = [dict(w, text=''.join(c['text'] for c in label), x1=max(c['x1'] for c in label))] if label else []
    tok = [nums[0]]
    for c in nums[1:]:
        if c['x0'] - tok[-1]['x1'] < 1.5:
            tok.append(c)
        else:
            out.append(dict(w, text=''.join(x['text'] for x in tok), x0=tok[0]['x0'], x1=tok[-1]['x1'])); tok = [c]
    out.append(dict(w, text=''.join(x['text'] for x in tok), x0=tok[0]['x0'], x1=tok[-1]['x1']))
    return out

def main(pdf_path, sc, version, outdir='.', suffix=''):
    freq_rows, var_rows = [], []
    var = None          # current variable (dict)
    cols = None         # current table columns: list of (name, x1)
    label_x_max = None  # words left of this belong to the label

    def close_var():
        if var is not None:
            var_rows.append({k: var[k] for k in
                ['dataset', 'variable', 'varlabel', 'has_table', 'by_wave', 'n_rows', 'omitted_values']})

    with pdfplumber.open(pdf_path) as pdf:
        for pno, page in enumerate(pdf.pages):
            if pno % 250 == 0:
                print(f'page {pno + 1} / {len(pdf.pages)}', file=sys.stderr)
            words = page.extract_words(x_tolerance=1.0, extra_attrs=['fontname', 'size'])
            chars = [dict(text=c['text'], x0=c['x0'], x1=c['x1'], top=c['top']) for c in page.chars]
            page.close()   # free memory: the codebook has 5,000+ pages
            dataset = None
            cols = None   # the table header is repeated on every page
            for line in lines_of(words):
                text = ' '.join(w['text'] for w in line)
                size = max(w['size'] for w in line)
                font = line[0]['fontname']

                # page header "17 spEmp" and page number: bold italic, 12.9 pt (SC6) or
                # 14.3 pt (other codebooks); variable headers are bold, not italic
                if size > 12.5 and ('Italic' in font or size > 13):
                    if line[0]['top'] < 40 and len(line) >= 2:
                        dataset = line[1]['text']
                    continue

                # variable header: bold line starting at x = 52 pt with a variable name
                # (10.8 pt in SC6, 12 pt in the other codebooks). Not: bold question
                # texts (x = 48.5), tables of contents (dot leaders, x = 49-50), and the
                # reading guide on the first pages (no dataset header).
                if ('Bold' in font and dataset and 10.5 < size < 12.5
                        and 51 < line[0]['x0'] < 54 and VARNAME.match(line[0]['text'])
                        and not LEADERS.search(text)):
                    close_var()
                    var = dict(dataset=dataset, variable=line[0]['text'],
                               varlabel=norm(' '.join(w['text'] for w in line[1:])),
                               has_table=1, by_wave=0, n_rows=0, omitted_values=0)
                    continue
                if var is None:
                    continue

                squeezed = text.replace(' ', '')
                if squeezed in ('keineTabelleerzeugt', 'notablegenerated'):
                    var['has_table'] = 0
                    continue
                if 'ausgelassen' in text or 'omitted' in text:
                    m = re.search(r'(\d+)', text)
                    if m:
                        var['omitted_values'] += int(m.group(1))
                    continue

                # table header: "Label  Wert  #  1  2 ... 17" (en: "Label  Code  #  ...") in SourceCodePro
                if 'SourceCode' in font and line[0]['text'] == 'Label':
                    cols = []
                    for w in line[1:]:
                        name = {'Wert': 'value', 'Code': 'value', '#': 'n_total'}.get(w['text'], 'w' + w['text'])
                        cols.append((name, w['x1']))
                    label_x_max = min(x for _, x in cols) - 30
                    if any(c[0].startswith('w') for c in cols):
                        var['by_wave'] = 1
                    continue
                if 'SourceCode' in font:      # "nach Welle" / "by wave"
                    continue
                if cols is None:              # question text etc.
                    continue

                # data row or continuation of a label
                # numbers are right-aligned under their column header; wide
                # numbers can stick out a few points, so take the nearest free column
                label_words, cells = [], {}
                # +10, not +30: an overprinted word can end exactly at the value column edge
                if any(w['x1'] > label_x_max + 10 and not NUM.match(norm(w['text'])) for w in line):
                    line = [p for w in line for p in
                            (split_overprinted(w, chars, label_x_max + 10)
                             if w['x1'] > label_x_max + 10 and not NUM.match(norm(w['text'])) else [w])]
                for w in line:
                    t = norm(w['text'])
                    if w['x0'] > label_x_max and (NUM.match(t) or t == '.'):
                        free = [c for c in cols if c[0] not in cells]
                        col = min(free, key=lambda c: abs(c[1] - w['x1'])) if free else None
                        if col is not None and abs(col[1] - w['x1']) < 14:
                            cells[col[0]] = t
                            continue
                    label_words.append(w)
                label = norm(' '.join(w['text'] for w in label_words))
                if 'value' in cells:
                    var['n_rows'] += 1
                    italic = any('Italic' in w['fontname'] for w in label_words)
                    v = cells['value']
                    row = dict(dataset=var['dataset'], variable=var['variable'], row=var['n_rows'],
                               value=v, label=label,
                               missing=int(italic or v == '.' or v.startswith('-')),
                               n_total=cells.get('n_total', ''))
                    for k, val in cells.items():
                        if k.startswith('w'):
                            row[k] = val
                    freq_rows.append(row)
                elif label and freq_rows and freq_rows[-1]['variable'] == var['variable']:
                    # wrapped label: "Hilfsarbei-" + "ter, Hilfskraft"
                    prev = freq_rows[-1]['label']
                    first = label.split(' ')[0]
                    if prev.endswith('-') and first not in ('oder', 'und', 'bzw.', 'sowie', 'bis', 'or', 'and', 'to'):
                        freq_rows[-1]['label'] = prev[:-1] + label     # "Hilfsarbei-" + "ter"
                    else:
                        freq_rows[-1]['label'] = prev + ' ' + label    # "Haupt-" + "oder"
    close_var()

    os.makedirs(outdir, exist_ok=True)
    sfx = f'_{suffix}' if suffix else ''
    f1 = os.path.join(outdir, f'codebook_freq_{sc}_{version}{sfx}.csv')
    f2 = os.path.join(outdir, f'codebook_vars_{sc}_{version}{sfx}.csv')
    max_wave = max([int(k[1:]) for r in freq_rows for k in r if k.startswith('w')] or [0])
    fields = ['dataset', 'variable', 'row', 'value', 'label', 'missing', 'n_total'] + [f'w{i}' for i in range(1, max_wave + 1)]
    with open(f1, 'w', newline='', encoding='utf-8-sig') as f:
        wr = csv.DictWriter(f, fieldnames=fields, restval=''); wr.writeheader(); wr.writerows(freq_rows)
    with open(f2, 'w', newline='', encoding='utf-8-sig') as f:
        wr = csv.DictWriter(f, fieldnames=list(var_rows[0].keys())); wr.writeheader(); wr.writerows(var_rows)
    print(f'{len(var_rows)} variables, {len(freq_rows)} value rows -> {f1}, {f2}', file=sys.stderr)

if __name__ == '__main__':
    main(*sys.argv[1:])
