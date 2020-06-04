
#
#-------------DANE----------#
A_BramaGWi<-c(180, 340, 460)
#A_BramaGWi[3] =320   #Natezenie w Erlangach
RT_IP<-c(11000, 13000, 17000)
#Intensywnoœæ pakietów 1/s, Klasy ruchu 'Real Time', 
NRT_IP<-c(12000, 16000, 14000)   #Intensywnoœæ pakietów 1/s, Klasy ruchu NRT

#IP-VBR1(RT - mowa), kodek G.729, RTP/UDP/IP/SDH
lRT = 10;   #œrednia d³ugoœæ pakietu [B]
Tpak_G729 = 20; #[ms]
Tpak_G729_s = 0.02;    #[s]

#IP-VBR2(NRT), TCP/IP/SDH
lNRT = 1600;    #œrednia d³ugoœæ pakietu [B]

#PSTN/ISDN/GSM na VBR1(RT), kodek G.711, RTP/UDP/IP/SDH
Tpak_G711 = 10; #[ms]
Tpak_G711_s = 0.01;  #[s]

WZ_RT <-matrix(0.2,6,6)
WZ_RT[1,1]=0
WZ_RT[1,2]=0.1
WZ_RT[1,3]=0.4
WZ_RT[1,6]=0.1
WZ_RT[2,2]=0
WZ_RT[3,1]=0.1
WZ_RT[3,2]=0.1
WZ_RT[3,3]=0
WZ_RT[3,4]=0.4
WZ_RT[4,1]=0.1
WZ_RT[4,4]=0
WZ_RT[4,5]=0.3
WZ_RT[5,2]=0.3
WZ_RT[5,3]=0.1
WZ_RT[5,5]=0
WZ_RT[6,2]=0.3
WZ_RT[6,5]=0.1
WZ_RT[6,6]=0



WZ_NRT<-matrix(0,6,6)
WZ_NRT[4,5]=0.6
WZ_NRT[4,6]=0.4
WZ_NRT[5,4]=0.4
WZ_NRT[5,6]=0.6
WZ_NRT[6,4]=0.6
WZ_NRT[6,5]=0.4


K1 = 4; #pakietów
K2 = 31;  #pakietów

IPNAG_IP = 20;  #[B],   d³ugoœæ nag³ówka IP
UDPNAG_UDP = 8; #[B],   d³ugoœæ nag³ówka UDP    
RTPNAG_RTP = 12;    #[B],   d³ugoœæ nag³ówka RTP


#
#Obliczenie NINi oraz NDSP_INi
#1
#prawdopodobieñstwo straty jest równe 0.002
#Obliczanie N na podstawie E1,N
NINi<-c(0,0,0) #Liczba ³¹czy wejœciowych

for (i in c(1:3)) {
  A = A_BramaGWi[i];
  E1 = 1;
  E1N1 = 1;
  while (E1 > 0.002){
    NINi[i] = NINi[i]+1;
    E1 = (A*E1N1)/(NINi[i] + A*E1N1);
    E1N1 = E1; 
  }
  
}

#2
#Przeliczanie NIni na NPCM, dla kierunku IN
NPCM_30_32_INi<-c(0,0,0)  #Liczba ³¹czy wejœciowych w PCM, zaookr¹glona w górê

for (i in c(1:3)){
  NPCM_30_32_INi[i] = ceiling(NINi[i]/30);
  
}

#3
#Rzeczywista liczba NR_INi, dla kierunku IN
NR_INi<-c(0,0,0)   #Rzeczywista licza ³¹czy wejœciowych 

for (i in c(1:3)){
  NR_INi[i] = NPCM_30_32_INi[i] * 30;
  
}

#4

nDSP = 4;   #Liczba strumieni ob³usgiwanych przez jeden procesor sygna³owy
NDSP_INi<-c(0,0,0) #Iloœæ procesorów sygna³owych dla kierunku IN

for (i in c(1:3)) {
  NDSP_INi[i] = ceiling(NR_INi[i] / nDSP);
  
}


#
#---Obliczenia NOUTi oraz NDSP_OUTi
#
#1
#Obliczenie liczby wyposa¿eñ na wyjœciu bramy GW do PSTN/ISDN/GSM
AOUT_PSTN_d<-c(0,0,0)
lambda_G729 = 1/Tpak_G729_s;
Ai_k<-c(0,0,0,0,0,0)

#Uzupe³nienie Ai_k, pierwsze 3 to PSTN, a pozosta³e 3 z IP

for(i in c(1:3)){
  Ai_k[i] = A_BramaGWi[i];
}
for(i in c(1:3)){
  Ai_k[i+3] = RT_IP[i]/lambda_G729;
}

#Oblicznie Aout
for (i in c(1:3)){   #Iloœæ PSTN
  for (k in c(1:6)){
    AOUT_PSTN_d[i] = AOUT_PSTN_d[i] + (WZ_RT[k,i]*Ai_k[k]);
  }
}

#2
#Obliczenia NOUTi dla AOUT 
NOUTi<-c(0,0,0)  ##Liczba ³¹czy wyjœciowych

for (i in c(1:3)){
  A = AOUT_PSTN_d[i];
  E1 = 1;
  E1N1 = 1;
  while (E1 > 0.002){
    NOUTi[i] = NOUTi[i]+1;
    E1 = (A*E1N1)/(NOUTi[i] + A*E1N1);
    E1N1 = E1; 
  }
  
}

#3
#Przeliczonie NOUTi z Gw na PCM30/32
NPCM_30_32_OUTi<-c(0,0,0)   #Liczba ³¹czy wyjœciowych w PCM, zaookr¹glona w górê


for (i in c(1:3)){
  NPCM_30_32_OUTi[i] = ceiling(NOUTi[i]/30);
  
}

#4
#Rzeczywista liczba NR ³¹czy na kierunku OUT
NR_OUTi <-c(0,0,0)   #Rzeczywista licza ³¹czy wejœciowych 

for (i in c(1:3)){
  NR_OUTi[i] = NPCM_30_32_OUTi[i] * 30;
  
}

#5
#Obliczenie DSP dla kierunku OUT, przy za³o¿eniu ¿e ndsp = 4
NDSP_OUTi  <-c(0,0,0) #Iloœæ procesorów sygna³owych dla kierunku IN

for(i in c(1:3)){
  NDSP_OUTi[i] = ceiling(NR_OUTi[i] / nDSP);
  
}


#
#------------Obliczenia CRT_IP_i -----------------------------------------#
#
lB_pakiet = Tpak_G711_s/(125*10^(-6));  #[B], w mianowniku 125us, 
lBnag = IPNAG_IP + UDPNAG_UDP + RTPNAG_RTP; #[B]
bcal_G711 = 8*(lB_pakiet + lBnag);  #[b]
lambda_G711 = 1/Tpak_G711_s;  #[1/s]

#chcemy stworzyæ 4 macierze i je po³¹czyæ z powrotem w jedn¹
#powód: dla ka¿dej æwiartki inne kodowanie
#
# krok 1 i 3   | krok 2
# G.711        | G.711->G.729
# PSTN->PSTN   | PSTN->IP
# --------------------------------------
# krok 4       | liczone poza konkursem
#              | (uzupe³niane na koniec)
# G.729->G.711 | G.729
# IP->PSTN     | IP->IP
#
#
#Przed doliczeniem Prawej dolnej æwiartki wynik bêdzie taki:
# 11 12 13 | 14 15 16
# 21 22 23 | 24 25 26
# 31 32 33 | 34 35 36
# -------------------
# 41 42 43 |  0  0  0
# 51 52 53 |  0  0  0
# 61 62 63 |  0  0  0
#
#a ostatecznie:
# 11 12 13 | 14 15 16
# 21 22 23 | 24 25 26
# 31 32 33 | 34 35 36
# -------------------
# 41 42 43 | 44 45 46
# 51 52 53 | 54 55 56
# 61 62 63 | 64 65 66
#
#Czyli bardzo podobnie jak zwyk³a macierz zainteresowañ

#1
#Obliczamy cRT_PSTN,i,j


cRT_PSTN_z_d<-matrix(0,3,3)


#Macierz Lewa górna
#PSTN->PSTN
for (i in c(1:3)) { #Sieæ PSTN wejœciowa
  for (k in c(1:3)){  #Sieæ PSTN docelowa
    cRT_PSTN_z_d[i,k] = (WZ_RT[i,k]*NR_INi[i]*lambda_G711*bcal_G711)/10^(6); #[Mb/s]
    
  }
}

#2
#Obliczamy cRT_PSTN_i,j
lambda_G729 = 1/Tpak_G729_s;  #[1/s]
lB_pakiet_IP = 10;  #[B], 
bcal_G729 = 8*(lB_pakiet_IP + lBnag);  #[b]


cRT_PSTN_z_IP_d<-matrix(0,3,3)


#Macierz Prawa górna
#PSTN->IP
for (i in c(1:3)){  #Sieæ PSTN Ÿród³owa
  for (k in c(4:6)) {   #Sieæ IP docelowa
    cRT_PSTN_z_IP_d[i,k-3] = (WZ_RT[i,k]*NR_INi[i]*lambda_G729*bcal_G729)/10^(6);
  }
}

#3
#Obliczamy cRT_jmPSTN_i

#Macierz ta sama co pierwsza - Lewa górna
#No jakby maceirz liczona w drug¹ sronê PSTN<-PSTN, ale to bez sensu, bo to to samo
#Zostawione jako ciekawostka do dalszych przemyœleñ. Nie u¿ywamy jej.
cRT_PSTN_d_z<-matrix(0,3,3)
for(i in c(1:3)){   #Sieæ PSTN docelowa
  for(k in c(1:3)) { #Sieæ PSTN Ÿród³owa
    cRT_PSTN_d_z[k,i] = (WZ_RT[k,i]*NR_INi[k]*lambda_G711*bcal_G711)/10^(6); 
    
  }
}

#4
#Obliczamy cRTj,PSTNi

#Macierz Lewa dolna
#IP->PSTN
cRT_PSTN_d_IP_z<-matrix(0,3,3)
for (i in c(1:3)) { #Sieæ PSTN docelowa
  for (k in c(4:6)){ #Sieæ IP Ÿród³owa
    cRT_PSTN_d_IP_z[k-3,i] = (WZ_RT[k,i]*RT_IP[k-3]*bcal_G729)/10^(6); #'-3' ¿eby macierz by³¹ 3x3 a nie 3x6
    
  }
}
cRT<-rbind(c(cRT_PSTN_z_d[1,],cRT_PSTN_z_IP_d[1,]),c(cRT_PSTN_z_d[2,],cRT_PSTN_z_IP_d[2,]),c(cRT_PSTN_z_d[3,],cRT_PSTN_z_IP_d[3,]),c(cRT_PSTN_d_IP_z[1,],c(0,0,0)),c(cRT_PSTN_d_IP_z[2,],c(0,0,0)),c(cRT_PSTN_d_IP_z[3,],c(0,0,0)))
#cRT = [cRT_PSTN_z_d cRT_PSTN_z_IP_d; cRT_PSTN_d_IP_z zeros(3,3)];   #po³¹czone macierze policzone w punktach od 1 do 4 



#Zamiast tworzyæ macierz Praw¹ doln¹ od razu wype³niamy na ¿ywca
#IP->IP
for (i in c(4:6)) {  #Sieæ IP Ÿród³owa
  for (k in c(4:6)) {  #Sieæ IP docelowa
    cRT[i,k] = (WZ_RT[i,k]*RT_IP[i-3]*bcal_G729)/10^(6);
    
  }
}


KierunekA_B<-c(0,0,0)  #Macierz dla kierunku A -> B
KierunekB_A<-c(0,0,0)  #MAcierz dla kierunku B -> A
cRT_IP<-c(0,0,0)   #suma przep³ywnoœci

for (i in c(1:3)){
  for (j in c(1:6)){
    KierunekA_B[i] = KierunekA_B[i] + cRT[i,j];
    KierunekB_A[i] = KierunekB_A[i] + cRT[j,i];
  }
  cRT_IP[i] = KierunekA_B[i] + KierunekB_A[i];
  
}






#Zadanie 1.2
#Obliczanie parametrów jakoœciowych na ustalonej drodze -IPLR,IPDT,IPDVmax




C = 150; #[Mb/s]
V = 200000;  #[Km/s]
RB1_RB2_S<-c(30,70,70,60)    #[km]
RB5_RB4_S<-c(50,40)
bcal_RT = 8*(lRT + lBnag);
bcal_NRT = 8*(lNRT + lBnag);    #bcal dla drogi NRT

RB1_RB2<-c(0,0,0,0)

RB1_RB2[1] = cRT[1,2] + cRT[1,3] + cRT[1,4] + cRT[1,5] + cRT[1,6] + cRT[2,1] + cRT[3,1] + cRT[4,1] + cRT[5,1] + cRT[6,1];
RB1_RB2[2] = cRT[1,2] + cRT[1,3] + cRT[1,6] + cRT[2,1] + cRT[3,1] + cRT[6,1];
RB1_RB2[3] = cRT[1,2] + cRT[1,6] + cRT[4,6] + cRT[5,6] + cRT[6,1] + cRT[6,4] + cRT[6,5];
RB1_RB2[4] = cRT[1,2] + cRT[3,2] + cRT[6,2] + cRT[2,3] + cRT[2,6];

RB1_RB2_ART<-c(0,0,0,0) 

RB1_RB2_ART[1] = RB1_RB2[1]/C;
RB1_RB2_ART[2] = RB1_RB2[2]/C;
RB1_RB2_ART[3] = RB1_RB2[3]/C;
RB1_RB2_ART[4] = RB1_RB2[4]/C;



RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT<-matrix(0,4,5)

for (i in c(1:4)) {  #Obliczanie elemenów tabeli z IPLR_ETocz_ETnad_Etprop_IPDT
  
  A = RB1_RB2_ART[i];
  RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,1] = ((1 - A)/(1 - (A^(K1+2))))*(A^(K1+1));
  RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,4] = RB1_RB2_S[i]/V;
  RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,3] = bcal_G711/(C*10^6);   
  
  mi = 1/RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,3];
  X = (A/mi);
  Y = (1+(A^K1)*(K1*A - (K1+1)));
  Z = (1 - A)*(1 - (A^(K1+2)));
  
  RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,2] = (X*Y)/Z;
  
  RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,5] = RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,2] + RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,3] + RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,4];
}


RB1_RB2_IPDTmax_IPDTmin_IPDVmax<-matrix(0,4,3)


for (i in c(1:4)){
  
  Et_NRT = bcal_NRT/(C*10^6);
  
  RB1_RB2_IPDTmax_IPDTmin_IPDVmax[i,2] = RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,3];
  RB1_RB2_IPDTmax_IPDTmin_IPDVmax[i,1] = (K1 + 1)*RB1_RB2_IPDTmax_IPDTmin_IPDVmax[i,2] + Et_NRT;  
  RB1_RB2_IPDTmax_IPDTmin_IPDVmax[i,3] = RB1_RB2_IPDTmax_IPDTmin_IPDVmax[i,1] - RB1_RB2_IPDTmax_IPDTmin_IPDVmax[i,2];
  
}


#Parametry jakoœciowe klasy RT


IPLR = 0;
IPDT = 0;
IPDV_MAX = 0;



for (i in c(1:4)){
  IPLR = IPLR + RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,1];
  IPDT = IPDT + RB1_RB2_IPLR_ETocz_ETnad_Etprop_IPDT[i,5];
  IPDV_MAX = IPDV_MAX + RB1_RB2_IPDTmax_IPDTmin_IPDVmax[i,3];
}




cNRT<-matrix(0,6,6)

for (i in c(4:6)) {
  for (j in c(4:6)) {
    cNRT [i,j] = (WZ_NRT[i,j]*NRT_IP[i-3]*bcal_NRT)/10^(6);
    
  }
}


RB5_RB4<-matrix(0,2,2)

RB5_RB4[1,1] = cRT[5,1] + cRT[5,4] + cRT[1,5] + cRT[4,5];
RB5_RB4[1,2] = cNRT[5,4];
RB5_RB4[2,1] = cRT[1,4] + cRT[5,4] + cRT[4,1] + cRT[4,5];
RB5_RB4[2,2] = cNRT[5,4];

RB5_RB4_ART_ANRT<-matrix(0,2,2)

for (i in c(1:2)) {
  RB5_RB4_ART_ANRT[i,1] = RB5_RB4[i,1]/C;
  RB5_RB4_ART_ANRT[i,2] = RB5_RB4[i,2]/(C-RB5_RB4[i,1]);
}



RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax<-matrix(0,2,5)
#Parametry jakoœciowe klasy NRT !!!!!!!
IPLR_NRT = 0;
IPDT_NRT = 0;
IPDV_MAX_NRT = 0;

Et_ocz = c(0,0);
Et_prop = c(0,0);

for(i in c(1:2))  {
  A = RB5_RB4_ART_ANRT[i,2]; #A NRT
  A1 = RB5_RB4_ART_ANRT[i,1]; #A RT
  RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,1] = ((1 - A)/(1 - (A^(K2+2))))*(A^(K2+1));
  
  ET_nad =  bcal_NRT/(C*10^6);
  mi = 1/ET_nad;
  X = (A/mi);
  Y = (1+(A^K2)*(K2*A - (K2+1)));
  Z = (1 - A)*(1 - (A^(K2+2)));
  
  Et_ocz[i] = (X*Y)/Z;
  Et_prop[i] = RB5_RB4_S[i]/V;
  
  RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,2] = ET_nad + Et_ocz[i] + Et_prop[i];
  
  RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,4] = ET_nad;
  RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,3] = (K2+1)*ET_nad/(1-A1);
  
  RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,5] = RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,3] - RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,4];
  
  #Sumowanie wartoœci 
  IPLR_NRT = IPLR_NRT + RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,1];
  IPDT_NRT = IPDT_NRT + RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,2];
  IPDV_MAX_NRT = IPDV_MAX_NRT + RB5_RB4_IPLR_IPDT_IPDTmax_IPDT_min_IPDVmax[i,5];
  
}


