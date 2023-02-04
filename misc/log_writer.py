import time
cnt=0
filename = r'log.txt'

for i in range(100):
    f = open(filename, 'a')
    for j in range(10):
        cnt+=1
        time.sleep(1)
        f.write(str(cnt)+'\n')
        f.flush()
        print('write', cnt)
    f.close()
    print('paise 3 sec')
    time.sleep(3)
