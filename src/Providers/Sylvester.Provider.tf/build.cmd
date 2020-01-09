@echo off
cd src\Sylvester.Provider.tf.Runtime
echo Building x64 Debug...
dotnet build -c Debug -p:Platform=x64
copy ..\Sylvester.Provider.tf.Designtime\bin\x64\Debug\net45\Sylvester.Provider.tf.Designtime.dll .\bin\x64\Debug\net45

echo Building x64 Release...
dotnet build -c Release -p:Platform=x64
copy ..\Sylvester.Provider.tf.Designtime\bin\x64\Release\net45\Sylvester.Provider.tf.Designtime.dll .\bin\x64\Release\net45
cd ..\..