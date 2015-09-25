class Command:
    cnt=0
    def run(self):
        self.cnt=self.cnt+1
        print('my1: ', self.cnt)
        