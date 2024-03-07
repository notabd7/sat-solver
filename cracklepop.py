#Write a program that prints out the numbers 1 to 100 (inclusive). If the number is divisible by 3, print Crackle instead of the number.
# If it's divisible by 5, print Pop. If it's divisible by both 3 and 5, 
#print CracklePop. You can use any language.

def cracklePop():
    for n in range(1, 101):
        if (n % 3 == 0):
            print("Crackle")
        if (n % 5 == 0):
            print("Pop")
        if ((n % 3 == 0) and (n % 5 == 0) ):
            print("CracklePop")
        else:
            print(n)

cracklePop()
    