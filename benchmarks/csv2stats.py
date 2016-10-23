import csv
inf = float("Inf")

TYPE = 'html' #'html' 'js' 'all'
MODE = 'def' #'par' 'def'
SUFFIX = TYPE + '_' + MODE
TIMEOUT = 600
TIEBREAK = False #False #True

reader = csv.reader(open('results_' + SUFFIX + '.csv'), delimiter = '|')
score = {}
for row in reader:
  #if int(row[3]) > 3:
    #continue
  inst = row[0]
  if inst not in score:
    score[inst] = {}
  status = row[1]
  domain = row[2] + '_' + row[3]
  if status == 'Normal':
    sc = int(row[4]) + int(row[5]) + int(row[6])
    it = int(row[7])
    time = float(row[9])
  else:
    sc = it = inf
    time = TIMEOUT
  score[inst][domain] = (sc, it, status, time)

domains = sorted(score.items()[0][1].keys())
n = len(domains)
results = dict((d, 0) for d in domains)
times = dict((d, 0.0) for d in domains)
fails = dict((d, 0) for d in domains)
all_fails = 0
better = dict((i, dict((j, 0) for j in range(0, n))) for i in range(0, n))
for inst, item in score.items():
  fail = True
  for i in range(0, n):
    d_i = domains[i]
    s_i = item[d_i][0]
    time = item[d_i][3]
    times[d_i] += time
    if time == TIMEOUT:
      fails[d_i] += 1
    else:
      fail = False
    for j in range(i + 1, n):
      ij = ji = False
      d_j = domains[j]
      s_j = item[d_j][0]
      if s_i == inf:
        if s_j < inf:
          results[d_j] += 1
          ji = True
          better[j][i] += 1
      else:
        if s_j == inf or s_i < s_j:
          results[d_i] += 1
          ij = True
          better[i][j] += 1
        elif s_i == s_j:
          if TIEBREAK:
            x = float(item[d_i][1] +  item[d_j][1])
            x_i = item[d_j][1] / x
            x_j = item[d_i][1] / x
            results[d_i] += x_i
            results[d_j] += x_j
            #if x_i != 0.5:
              #print d_i, d_j, inst
            if x_i > x_j:
              ij = True
              better[i][j] += 1
            elif x_j > x_i:
              ji = True
              better[j][i] += 1
          else:
            results[d_i] += 0.5
            results[d_j] += 0.5
        else:
          results[d_j] += 1
          better[j][i] += 1
          ji = True
      k_i = d_i[d_i.rfind('_'):]
      k_j = d_j[d_j.rfind('_'):]
      if MODE == 'def' or k_i == k_j:
        if ij:
          if d_i.startswith('no,co') and d_j.startswith('sf'):
            print ' *** NO x CO better than SAFE for', inst, k_i
          elif d_i.startswith('js') and d_j.startswith('sf'):
            print ' *** JSAI better than SAFE for',inst, k_i
          elif d_i.startswith('ci,no,ss') and d_j.startswith('hy'):
            print ' *** HY* better than HY for',inst, k_i
          elif d_i.startswith('ci,no,ss') and d_j.startswith('js'):
            print ' *** HY* better than JSAI for',inst, k_i
        elif ji:
          if d_i.startswith('co') and d_j.startswith('ss'):
            print ' *** SS than CO for', inst, k_i
          if d_i.startswith('ci,co') and d_j.startswith('hy'):
            print ' *** Hybrid better than CI x CO for', inst, k_i
          if d_i.startswith('ss,ci') and d_j.startswith('hy'):
            print ' *** Hybrid better than SS x CI for', inst, k_i  
          elif d_i.startswith('js') and d_j.startswith('sf'):
            print ' *** SAFE better than JSAI for',inst, k_i
          elif d_i.startswith('hy') and d_j.startswith('ss,ci'):
            print ' *** SS x CI better than Hybrid for',inst, k_i
          elif d_i.startswith('ci,co') and d_j.startswith('ci,no,ss'):
            print ' *** HY* better than CI for',inst, k_i
    assert better[i][i] == 0
  if fail:
    all_fails += 1
assert fails['ci,no,ss_3'] == all_fails
m = len(score)
print 'Processed',m,'problems --- ',all_fails,'of them not analysable by any domain'
print 'Score',sorted(results.items(), key = lambda x : x[1], reverse = True)
print 'Time',[(x[0], round(x[1] / m, 2)) for x in sorted(times.items(), key = lambda x : x[1])]
print '# Fails',sorted(fails.items(), key = lambda x : x[1])
print domains
#for i in range(0, n):
  #print domains[i],',',
  #for j in range(0, n):
    #print better[i][j], ',',
  #print
#print 'score'
for d, s in sorted(results.items(), reverse = True):
  print d, s
print 'time'
for d, s in sorted(times.items(), key = lambda x : x[1]):
  print d, s/m
print 'fails'
for d, s in sorted(fails.items()):
  print d, s