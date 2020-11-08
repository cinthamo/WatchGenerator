@echo off
cd "C:\Users\cristian\Google Drive\Estudios\Cursos Actuales\Tesis Maestria\src\templates\control"
echo Controls
if not exist                            E:\dev\AndroidFlexibleClientMaestria\Workspace\Controls\src\main\res\layout md E:\dev\AndroidFlexibleClientMaestria\Workspace\Controls\src\main\res\layout
copy Controls\main.xml                  E:\dev\AndroidFlexibleClientMaestria\Workspace\Controls\src\main\res\layout\
copy Controls\MainNew.java              E:\dev\AndroidFlexibleClientMaestria\Workspace\Controls\src\main\java\com\artech\masterwatch\controls\
copy MainBase.java                      E:\dev\AndroidFlexibleClientMaestria\Workspace\Controls\src\main\java\com\artech\masterwatch\
copy MyImageView.java                   E:\dev\AndroidFlexibleClientMaestria\Workspace\Controls\src\main\java\com\artech\masterwatch\
echo TextData
if not exist                            E:\dev\AndroidFlexibleClientMaestria\Workspace\TextData\src\main\res\layout md E:\dev\AndroidFlexibleClientMaestria\Workspace\TextData\src\main\res\layout
copy TextData\main.xml                  E:\dev\AndroidFlexibleClientMaestria\Workspace\TextData\src\main\res\layout\
copy TextData\MainNew.java              E:\dev\AndroidFlexibleClientMaestria\Workspace\TextData\src\main\java\com\artech\masterwatch\textdata\
copy MainBase.java                      E:\dev\AndroidFlexibleClientMaestria\Workspace\TextData\src\main\java\com\artech\masterwatch\
copy MyImageView.java                   E:\dev\AndroidFlexibleClientMaestria\Workspace\TextData\src\main\java\com\artech\masterwatch\
echo NumberList
if not exist                            E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\res\layout md E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\res\layout
copy NumberList\main.xml                E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\res\layout\
copy NumberList\line.xml                E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\res\layout\
copy NumberList\MainNew.java            E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\java\com\artech\masterwatch\numberlist\
if not exist                            E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\java\com\artech\masterwatch\numberlist\controls md E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\java\com\artech\masterwatch\numberlist\controls
copy NumberList\MyRecyclerView.java     E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\java\com\artech\masterwatch\numberlist\controls\
copy NumberList\MyRecyclerAdapter.java  E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\java\com\artech\masterwatch\numberlist\controls\
copy MainBase.java                      E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\java\com\artech\masterwatch\
copy MyImageView.java                   E:\dev\AndroidFlexibleClientMaestria\Workspace\NumberList\src\main\java\com\artech\masterwatch\
echo CountryList
if not exist                            E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\res\layout md E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\res\layout
copy CountryList\main.xml               E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\res\layout\
copy CountryList\line.xml               E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\res\layout\
copy CountryList\MainNew.java           E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\java\com\artech\masterwatch\countrylist\
if not exist                            E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\java\com\artech\masterwatch\countrylist\controls md E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\java\com\artech\masterwatch\countrylist\controls
copy CountryList\MyRecyclerView.java    E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\java\com\artech\masterwatch\countrylist\controls\
copy CountryList\MyRecyclerAdapter.java E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\java\com\artech\masterwatch\countrylist\controls\
copy MainBase.java                      E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\java\com\artech\masterwatch\
copy MyImageView.java                   E:\dev\AndroidFlexibleClientMaestria\Workspace\CountryList\src\main\java\com\artech\masterwatch\
pause