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
srand( time(NULL) ); //���� ���������������� ��������� ��������� ����� ���������� ��������� ������ (�������� - ������� ��������), ����� ������ ��� �� ����� �������� ���� � �� ��
magic=rand();
/*for (a=0; (magic<0)&&(magic>=100); a++) //���������� � � ����� �� ������������ ��� && ������, ��� ������������ ||
 {
 magic=rand();
   } */
magic = (rand() % 100)+1; //� ���� ���� ��������, �� ������-�� ��� �� ������. % - ����� ������� �� �������. ������� �� ������� ������ ����� �� 100 ������ ����� � �������� �� 0 �� 99.
//a=10; //�� ����� ������������� ������� �����, ����� ��� ����� ���� ����������� ����?
for(a=10;a!=0;--a) //��� ���
 {
   printf("vvedite vash variant: ");
   scanf("%d",&guess);
   getchar();
   if (guess==magic)
   {
   printf("\nPozdravlajy! Vi ygadali!\n");
   break; //goto - ���, ������������ ������ � ������ ������� �������������? break - ����� �� �������� �����
   }
   else if (guess>magic) printf("vi ne ygadali, vashe 4islo bol'she. Poprobuite eshe raz\n");
   else printf("vi ne ygadali, vashe 4islo men'she. Poprobyite eshe raz\n");
   }
printf("vam ne povezlo.. no mojet povezet v sledyjyshii raz ");
getchar ();
return 0;
}
