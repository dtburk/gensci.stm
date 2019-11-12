import nltk, os, collections, re, sys
from tqdm import tqdm

input_dir, output_dir, nes_dir = sys.argv[1:]

# If any dirs don't exist yet, create them
for DIR in [output_dir, nes_dir]:
    if not os.path.exists(DIR): 
        os.makedirs(DIR)

Report = collections.namedtuple("Report", ["name", "content"])

rpts = []

for fname in os.listdir(input_dir):
    with open(os.path.join(input_dir, fname), "r") as f:
        rpts.append(Report(name=fname, content=f.read()))

rpt_content = [x.content for x in rpts]

rpt_content = [re.sub(r'\n+| +', ' ', x) for x in rpt_content]

rpt_pages = [re.split('\f', x) for x in rpt_content]

for i, d in enumerate(rpt_pages):
    print("\nReport " + str(i + 1) + " of " + str(len(rpt_pages)))
    f1 = open(os.path.join(nes_dir, rpts[i].name), 'w')
    f2 = open(os.path.join(output_dir, rpts[i].name), 'w')
    for p in tqdm(d):
        p = nltk.sent_tokenize(p)
        for s in p:
            # print("Sentence " + str(j))
            chunked_sent = nltk.ne_chunk(nltk.pos_tag(nltk.word_tokenize(s)))
            for c in chunked_sent:
                is_tree = isinstance(c, nltk.Tree)
                if is_tree and c.label() in ['ORGANIZATION', 'PERSON', 'GPE']:
                    f1.write(' '.join([x[0] for x in c]) + ',' + c.label() + '\n')
                else:
                    if is_tree:
                        f2.write(' '.join([x[0] for x in c]) + ' ')
                    else:
                        f2.write(c[0] + ' ')
        f2.write('\f')
    f1.close()
    f2.close()
