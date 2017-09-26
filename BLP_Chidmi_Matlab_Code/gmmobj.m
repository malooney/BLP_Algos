function [f,g]=gmmobj(theta2)
%function f=gmmobj(theta2)

global invA theta1 theti thetj X1 IV
%global    theta1 theti thetj X1 IV

delta=meanval(theta2);
%load invA
if max(isnan(delta))==1
    f=1e+10
else
    temp1=X1'*IV;
    temp2=delta'*IV;
    theta1=inv(temp1*invA*temp1')*temp1*invA*temp2';
    clear temp1 temp2
    gmmresid=delta-X1*theta1;
    temp1=gmmresid'*IV;
    f=temp1*invA*temp1';
    clear temp1
   % invA=inv(IV'*gmmresid*gmmresid'*IV);
    save gmmresid gmmresid
    %save invA invA
   
    g=gradobj(theta2);
end

disp(['GMM Objective:   ' num2str(f)])