#include <stdio.h>
#include <stdlib.h>
#include <time.h>
int main()
{
int guess;
int magic;
int a;
printf("programma generiryet sly4ainoe 4islo ot 1 do 100. vasha zada4a\n");
printf("ygadat' ego, na 4to vam daetsa vsego 10 popitok. itak, na4ali!\n");
srand( time(NULL) ); //надо инициализировать генератор случайных чисел достаточно рандомным числом (например - текущим временем), иначе всякий раз он будет генерить одно и то же
magic=rand();
/*for (a=0; (magic<0)&&(magic>=100); a++) //логическое И в языке си обозначается как && кстати, ИЛИ обозначается ||
 {
 magic=rand();
   } */
magic = (rand() % 100)+1; //я этот цикл исправил, но вообще-то так не делают. % - взять остаток от деления. остаток от деления любого числа на 100 всегда лежит в пределах от 0 до 99.
//a=10; //ну зачем устанавливать счётчик здесь, когда для этого есть специальное поле?
for(a=10;a!=0;--a) //вот так
 {
   printf("vvedite vash variant: ");
   scanf("%d",&guess);
   getchar();
   if (guess==magic)
   {
   printf("\nPozdravlajy! Vi ygadali!\n");
   break; //goto - зло, использовать только в случае КРАЙНЕЙ необходимости? break - выход из текущего цикла
   }
   else if (guess>magic) printf("vi ne ygadali, vashe 4islo bol'she. Poprobuite eshe raz\n");
   else printf("vi ne ygadali, vashe 4islo men'she. Poprobyite eshe raz\n");
   }
printf("vam ne povezlo.. no mojet povezet v sledyjyshii raz ");
getchar ();
return 0;
}
