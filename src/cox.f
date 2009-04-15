C Output from Public domain Ratfor, version 1.0
      subroutine initcx(n,y,kq, icensq,iriskq, ddq,tq)
      integer n,p
      real y(n), tq(n),x(n),sx(n)
      integer ddq(n),icensq(n),iriskq(n)
      i=1
23000 if(icensq(i).ne.1)then
      i=i+1
      goto 23000
      endif
23001 continue
      tq(1)=y(i)
      iriskq(1)=i
      jj=1
      i=i+1
23002 if(i.le.n)then
      if((y(i).ne.y(i-1)).and.(icensq(i).eq.1))then
      jj=jj+1
      tq(jj)=y(i)
      iriskq(jj)=i
      endif
      i=i+1
      goto 23002
      endif
23003 continue
      kq=jj
      call calcdd(n,ddq,kq,iriskq,icensq)
      return
      end
      subroutine calcdd(n,ddq,kq,iriskq,icensq)
      integer iriskq(n),icensq(n),ddq(n)
      do23006 i=1,kq 
      ddq(i)=0
      ii=iriskq(i)
      if(i.lt.kq)then
      ij=i+1
      ik=iriskq(ij)-1
      else
      ik=n
      endif
      do23010 j=ii,ik
      ddq(i)=ddq(i)+icensq(j)
23010 continue
23011 continue
23006 continue
23007 continue
      return
      end
      subroutine calcsc(beta,lam,n,x,sx,iriskq,kq,ddq,icensq, val,scrt1,
     *scrt2)
      integer n,iriskq(n),icensq(n),ddq(n)
      real x(n),sx(n),scrt1(n),scrt2(n),beta,val,lam
      do23012 i=1,kq
      ii=iriskq(i)
      scrt1(i)=0
      scrt2(i)=0
      do23014 j=ii,n 
      temp=exp(x(j)*beta)
      scrt1(i)=scrt1(i)+x(j)*temp
      scrt2(i)=scrt2(i)+temp
23014 continue
23015 continue
23012 continue
23013 continue
      val=0
      do23016 i=1,kq 
      val=val+sx(i)-ddq(i)*scrt1(i)/scrt2(i)
23016 continue
23017 continue
      if(beta.gt.0)then
      val=val-lam
      endif
      if(beta.lt.0)then
      val=val+lam
      endif
      return
      end
      subroutine calcsx(n,x,kq,iriskq,icensq,sx)
      integer n,iriskq(n),icensq(n),kq
      real x(n),sx(n)
      do23022 i=1,kq 
      sx(i)=0
      ii=iriskq(i)
      if(i.lt.kq)then
      ij=i+1
      ik=iriskq(ij)-1
      else
      ik=n
      endif
      do23026 j=ii,ik
      sx(i)=sx(i)+icensq(j)*x(j)
23026 continue
23027 continue
23022 continue
23023 continue
      return
      end
