function f=ind_sh(expmval,expmu)
% This function gives the individual(for each consumer and brand)
% probabilities

global ns cdindex cdid
eg=expmu.*kron(ones(1,ns),expmval);
temp=cumsum(eg);
sum1=temp(cdindex,:);
sum1(2:size(sum1,1),:)=diff(sum1);
denom1=1./(1+sum1);
denom=denom1(cdid,:);
clear temp sum1
f=eg.*denom;
