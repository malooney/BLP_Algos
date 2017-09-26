function f=meanval(theta2)

global theti thetj silent X2 s
load mvaold

if max(abs(theta2-oldt2)) < 0.01;
    tol=1e-9;
    flag=0;
else
    tol=1e-6;
    flag=1;
end

theta2w=full(sparse(theti,thetj,theta2));
expmu=exp(mufunc(X2,theta2w));

norm=1;
avgnorm=1;

i=0;

while norm > tol*10^(flag*floor(i/50))& avgnorm > 1e-3*tol*10^(flag*floor(i/50))
    
    mval=mvaold.*s./mktsh(mvaold,expmu);
    
    t=abs(mval-mvaold);
    norm=max(t);
    avgnorm=mean(t);
    mvaold=mval;
    i=i+1;
end
disp(['# of iterations for delta convergence:   ' num2str(i)])

if flag==1 & max(isnan(mval)) < 1;
    mvaold=mval;
    oldt2=theta2;
    save mvaold mvaold oldt2
end
f=log(mval);