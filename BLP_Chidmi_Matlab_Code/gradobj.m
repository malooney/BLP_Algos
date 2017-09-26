function df=gradobj(theta2)

global   invA IV
%global   IV
load gmmresid
load mvaold
%load invA
temp=jacob(mvaold, theta2)';
df=2*temp*IV*invA*IV'*gmmresid;
