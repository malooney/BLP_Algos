 % X1 contains price, promo, price reduction, pl dummy, fat content dummy,
 % and store dummy
% X2 contains constant, price, store dummies fat content dummies 

clear
clc
global    invA ns X1 X2 s IV vfull dfull theta1 theti thetj cdid cdindex
%global    ns X1 X2 s IV vfull dfull theta1 theti thetj cdid cdindex
load milkdata

IV=[I pr PL]; %Instrumental Variables

ns=20; % number of cps draw
nmkt=58; % number of markets
nbrn=6; % number of brands
cdid=kron([1:nmkt]',ones(nbrn,1)); % gives the market id
cdindex=[nbrn:nbrn:nbrn*nmkt]'; % indexes the markets
 
theta2w=[2.0682    2.1000    1.0473;
    1.5541    2.0352   -0.8324;
    0.6403    2.6775    1.3040;
   -0.3018    1.2227    3.4240;
    0.6605    3.1289    1.8451;
    1.0198    0.8942    1.3901];
   % 0.0515    1.1286    1.3066];
    %0.6613    0.6250   -2.5911];
   %0.6561    0.2886    0.2356;
   %0.01245 0.1235 0.3315];

     
[theti, thetj, theta2]=find(theta2w);

% horz=['         mean        sigma       Age     Income      Size        Under15'];
% vert=['constant';
%       'Price   ';
%       'Sugar   ';
%       'Puffed  '];
 %load invA 

 invA= inv(IV'*IV); 
 
  temp= cumsum(s);
  sum1= temp(cdindex,:);
  sum1(2:size(sum1,1),:)= diff(sum1);
  outshr= 1-sum1(cdid,:);
  y=log(s)-log(outshr);
  
  mid= X1'* IV* invA* IV';
  simple_logit= inv(X1'*X1)*X1'*y; % simple logit, no IV's
  ttt= inv(mid* X1)* mid* y; % simple logit, with IV's
  mvalold= X1* ttt;
  oldt2= zeros(size(theta2));
  mvaold= exp(mvalold);
  
  save mvaold mvaold oldt2
  %save invA invA
  
  clear mid y outshr oldt2 mvalold temp sum1 
  
  vfull=v(cdid,:);
  dfull=demogr(cdid,:);
  %tic
  %options=foptions;
  %options(2)=0.01;
  %options(3)=0.001;
  options=optimset('GradObj','on','TolFun',0.01,'TolX',0.01);
     
  [theta2,fval] = fminunc(@gmmobj, theta2, options)
  
  %theta2 = fmins('gmmobj',theta2)
  %comp_t=toc/60;
%  disp(['GMM objective:  ' num2str(options(8))])
%disp(['# of objective function evaluations:  ' num2str(options(10))])
%disp(['run time (minutes):  ' num2str(comp_t)])
%diary off

  vcov=var_cov(theta2);
  se=sqrt(diag(vcov));
  t = size(se,1) - size(theta2,1);
  se2w=full(sparse(theti,thetj,se(t+1:size(se,1))));
  theta=[theta1;theta2];
  tst=(theta./se);
  theta2w=full(sparse(theti,thetj,theta2));
  
  
  
 