function f=var_cov(theta2)

global  invA IV 
%global  IV
load milkdata
load mvaold
load gmmresid
%load invA


N=size(X1,1);
Z=size(IV,2);
temp=jacob(mvaold,theta2);
a=[X1 temp]'*IV;
IVres=IV.*(gmmresid*ones(1,Z));
b=(IVres'*IVres)/348;
f=inv(a*invA*a')*a*invA*b*invA*a'*inv(a*invA*a');
