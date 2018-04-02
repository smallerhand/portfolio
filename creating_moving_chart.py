#y1년부터 y2년까지 해별 상위 n개 이름 그래프로 저장하는 name이라는 함수 만듬. 
#성별은 M, F 이외를 입력하면 전체로 만듬.
def Name(y1, y2, n, g):
    import pandas as pd
    from numpy import array
    import matplotlib.pyplot as plt
    import time as t
    from matplotlib import font_manager, rc
    rc('font', family='AppleGothic')
    plt.rcParams['axes.unicode_minus'] = False
    for y in range(y1, y2+1):
        df = pd.read_csv('desktop/python/names/yob'+str(y)+'.txt', names=['name','gender','birth'])
        if g in ["M","F"]:
            if g=="M":
                gender="남자 이름 1~"+str(n)+"위"
            else:
                gender="여자 이름 1~"+str(n)+"위"
            df = df[df['gender']==g]
        else:
            gender="전체 이름 1~"+str(n)+"위"
        a = df.sort_values(by="birth", ascending=False)[:n]
        a = a.set_index("name").loc[:,"birth"]
        a.plot(kind="bar", alpha=.5)
        plt.title(str(y)+'년 '+gender)
        plt.savefig('desktop/python/'+str(y)+str(g)+'.png', format="png", 
                    bbox_inches="tight")
        plt.clf()
        t.sleep(.02)

#위 함수 이용해서 그림 저장        
Name(1989, 2016, 10, "F")

#저장시킨 그림으로 애니메이션 만듬
import imageio
files=[]
for i in range(1989, 2017):
    files.append(str(i)+"F.png")
files
png_dir = "desktop/python/"
images = []
for i in files:
    file_path=png_dir+i
    images.append(imageio.imread(file_path))
imageio.mimsave(png_dir+'movie.gif', images, duration=.5)
c
