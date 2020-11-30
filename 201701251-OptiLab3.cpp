#include<bits/stdc++.h>
#define FAST ios_base::sync_with_stdio(0); cin.tie(0); cout.tie(0);

using namespace std;

void demandSupplyEqualization(vector<vector<int> > &cost_mat, vector<int> &supply, vector<int> &demand, int tot_supply, int tot_demand, int &rows, int &cols)
{
    if(tot_demand > tot_supply)
    {
        cout<<"DEMAND IS MORE THAN SUPPLY.\nONE DUMMY ROW ADDED\n";
        int dif = tot_demand - tot_supply;
        supply.push_back(dif);
        rows++;
        vector<int> dummy_row(cols,0);
        cost_mat.push_back(dummy_row);

    }
    else if(tot_demand < tot_supply)
    {
        cout<<"SUPPLY IS MORE THAN DEMAND.\nONE DUMMY COLUMN ADDED\n";
        int dif = tot_supply - tot_demand;
        demand.push_back(dif);
    cols++;
        vector<int> dummy_col;
        for(int i=0;i<rows;i++) 
     {
      cost_mat[i].push_back(0);
     }
    }
}

void VAM_Method(vector<vector<int> > &cost_mat, vector<int> &supply, vector<int> &demand, vector<vector<int> > &op_mat,
 vector<bool> &isDone_row, vector<bool> &isDone_col, vector<int> &row_min_diff, vector<int> &column_min_diff, int &last, int &tot_basic_vars)
{
 int rows = cost_mat.size(), cols = cost_mat[0].size();

 // Find difference between two min. elements row-wise
 while(last <= tot_basic_vars)
    {
        for(int i=0; i<rows; i++)
     {
         if(isDone_row[i])
         {
             row_min_diff[i]=-1;
         }
         else
         {
          vector<int> min_vals;
          for(int j=0;j<cols;j++)
          {
              if(!isDone_col[j])
              {
                  min_vals.push_back(cost_mat[i][j]);
              }
          }
          sort(min_vals.begin(),min_vals.end());
          if(min_vals.size() == 1)
          {
           row_min_diff[i]=min_vals[0];
          }
          else if(min_vals.size() >= 2)
          {
              row_min_diff[i]=min_vals[1]-min_vals[0];
          }
          else 
          {
           row_min_diff[i]=-1;
          }
      }
     }

     // Find difference between two min. elements column-wise
     for(int i=0; i<cols; i++)
     {
         if(isDone_col[i])
         {
             column_min_diff[i]=-1;
         }
         else
         {
          vector<int> min_vals;
          for(int j=0; j<rows; j++)
          {
              if(isDone_row[j]==false)
              {
                  min_vals.push_back(cost_mat[j][i]);
              }
          }
          sort(min_vals.begin(),min_vals.end());
          if(min_vals.size() == 1)
          {
           column_min_diff[i]=min_vals[0];
          }
          else if(min_vals.size() == 1)
          {
              column_min_diff[i]=min_vals[1]-min_vals[0];
          }
          else
          {
           column_min_diff[i]=-1;
          }
      }
     }

        int maxi = max(*max_element(row_min_diff.begin(),row_min_diff.end()), 
            *max_element(column_min_diff.begin(),column_min_diff.end()));

        int i_iter=-1, j_iter=-1, maximum=0;
        
        for(int i=0; i<rows; i++)
     {
         if(row_min_diff[i]==maxi)
         {
          int ind = -1, tmax=0, tmin=INT_MAX;

          for(int j=0; j<cols; j++)
          {
              if(!isDone_col[j])
                  tmin=min(tmin,cost_mat[i][j]);
          }
          for(int j=0; j<cols; j++)
          {
              if(tmin==cost_mat[i][j])
              {
                  int temp=min(supply[i], demand[j]);
                  if(tmax<temp)
                  {
                      tmax=temp;
                      ind=j;
                  }
              }
          }
             
             if(tmax>maximum)
             {
                 maximum=tmax;
                 i_iter=i;
                 j_iter=ind;
             }

         }
     }

     for(int i=0; i<cols; i++)
     {
         if(column_min_diff[i]==maxi)
         {
          int ind = -1, tmax=0, tmin=INT_MAX;

          for(int j=0; j<rows; j++)
          {
              if(isDone_row[j]==false)
                  tmin=min(tmin,cost_mat[j][i]);
          }
          for(int j=0; j<rows; j++)
          {
              if(tmin==cost_mat[j][i])
              {
                  int temp=min(supply[j], demand[i]);
                  if(tmax<temp)
                  {
                      tmax=temp;
                      ind=j;
                  }
              }
          }

             if(tmax>maximum)
             {
                 maximum=tmax;
                 i_iter=ind;
                 j_iter=i;
             }
         }
     }

        if(maximum == demand[j_iter])
        {
            int temp = supply[i_iter];
            supply[i_iter] -= maximum;
            demand[j_iter] = 0;
            isDone_col[j_iter] = true;
            column_min_diff[j_iter] = -1;
            if(maximum == temp)
            {
                supply[i_iter] = 0;
                demand[j_iter] -= maximum;
                isDone_row[i_iter] = true;
                row_min_diff[i_iter] = -1;
            }
        }
        else
        {
            supply[i_iter] = 0;
            demand[j_iter] -= maximum;
            isDone_row[i_iter] = true;
            row_min_diff[i_iter] = -1;
        }

        op_mat[i_iter][j_iter] = maximum;

        last++;

        bool f1=false;
        for(int i=0; i<rows; i++)
        {
            if(!isDone_row[i])
            {
             f1=true;
            }
        }
        for(int i=0; i<cols; i++)
        {
            if(!isDone_col[i])
            {
             f1=true;
            }
        }
        
        if(f1==false) break;
    }
}

void find_closed_loop(pair<int,int> loc, vector<vector<int>> &op_mat)
{
    int rows = op_mat.size(),cols = op_mat[0].size();
    int lc=0, mi=loc.first, mj=loc.second, i,j,k;
    vector<vector<int>> loop(rows,vector<int>(cols,0));
    vector<vector<char>> sign(rows,vector<char>(cols,'/'));

    for(i=0; i<rows; i++)
    {
      for(j=0; j<cols; j++)
      {
              if(op_mat[i][j]>0)
                 loop[i][j]=1;
              else
                 loop[i][j]=0;

              sign[i][j]='a';
         }
    }

    for(k=0; k<rows; k++)
    {
        for(i=0; i<rows; i++)
        {
            for(j=0; j<cols; j++)
            {
                if(loop[i][j]==1)
                    lc++;
            }
            if(lc==1 && i!=mi)
            {
                for(j=0; j<cols; j++)
                 loop[i][j]=0;
            }
          lc=0;
        }

        lc=0;
        for(j=0; j<cols; j++)
        {
            for(i=0; i<rows; i++)
            {
                if(loop[i][j]==1)
                 lc++;
            }
            if(lc==1 && j!=mj)
            {
                for(i=0; i<rows; i++)
                 loop[i][j]=0;
            }
            lc=0;
        }
    }

    loop[mi][mj]=1;
    sign[mi][mj]='+';
    i=mi;


 for(k=1; k<rows; k++)
 {
  
  for(j=0; j<cols; j++)
  {
   if(loop[i][j]==1 && sign[i][j]=='a')
   {
    sign[i][j]='-';
    break;
   }
  }
  for(i=0;i<rows;i++)
  {
   if(loop[i][j]==1 && sign[i][j]=='a')
   {
    sign[i][j]='+';
    break;
   }
  }
 }

 int mini = INT_MAX;
 for(int i=0; i<rows; i++)
 {
      for(int j=0; j<cols; j++)
      {
            if(sign[i][j] == '-')
                mini=min(mini,op_mat[i][j]);
         }
    }
    op_mat[mi][mj] = 0;
 for(int i=0; i<rows; i++)
 {
      for(int j=0; j<cols; j++)
      {
            if(sign[i][j] == '+')
            {
                op_mat[i][j] += mini;
            }
            else if(sign[i][j] == '-')
            {
                op_mat[i][j] -= mini;
                if(op_mat[i][j]==0)
                { 
                 op_mat[i][j] = -1;
                }
            }
         }
    }

}

void MODI_Method(vector<vector<int> > &cost_mat, vector<vector<int> > &op_mat)
{
 int rows = cost_mat.size(), cols=cost_mat[0].size();
 vector<int> u(rows,INT_MAX);
    vector<int> v(cols,INT_MAX);
    pair<int,char> p;

    int maxi1=0,ind1=-1,maxi2=0,ind2=-1;
    for(int i=0; i<rows; i++)
    {
        int count=0;
        for(int j=0; j<cols; j++)
        {
            if(op_mat[i][j]>=0) count++;
        }
        if(maxi1<count)
        {
            maxi1 = count;
            ind1 = i;
        }
    }
    for(int j=0; j<cols; j++)
    {
        int count=0;
        for(int i=0; i<rows; i++)
        {
            if(op_mat[i][j]>=0) count++;
        }
        if(maxi2<count)
        {
            maxi2 = count;
            ind2 = j;
        }
    }
    if(maxi1>maxi2) 
 {
  p = {ind1,'r'};
 }
 else
 {
  p = {ind2,'c'};
 }
    
    if(p.second=='r')
    {
     u[p.first] = 0;
    }
    else 
    {
     v[p.first] = 0;
    }

    while(1)
    {
        int count = 0;
        for(int i=0; i<rows; i++)
        {
            for(int j=0; j<cols; j++)
            {
                if(op_mat[i][j]>=0)
                {
                    if(u[i]!=INT_MAX && v[j]==INT_MAX)
                    {
                        v[j] = cost_mat[i][j] - u[i];
                    }
                    else if(u[i]==INT_MAX && v[j]!=INT_MAX)
                    {
                        u[i] = cost_mat[i][j] - v[j];
                    }
                }
            }
        }

        for(int i=0; i<rows; i++)
        {
            if(u[i] != INT_MAX) count++;
        }
        for(int j=0; j<cols; j++)
        {
            if(v[j] != INT_MAX) count++;
        }

        if(count == rows + cols) break;
    }

    vector<vector<int> > delta(rows,vector<int>(cols, INT_MAX));
    bool status = false;
    for(int i=0; i<rows; i++)
    {
        for(int j=0; j<cols; j++)
        {
            if(op_mat[i][j] < 0)
            {
                delta[i][j] = cost_mat[i][j] - (u[i] + v[j]);
                if(delta[i][j] < 0)
                {
                 status = true;
                }
            }
        }
    }
    if(status == false)
    {
        cout<<"NO NEED OF OPTIMIZATION,\nOBTAINED INITIAL BASIC FEASIBLE SOLUTION IS OPTIMUM.";
        return;
    }

    while(status)
    {
     int tmini=INT_MAX,tp=0,tq=0;
     for(int i=0; i<rows; i++)
     {
         for(int j=0; j<cols; j++)
         {
             if(delta[i][j] < 0)
             {
                 if(tmini > delta[i][j])
                 {
                     tmini = delta[i][j];
                     tp=i;
                     tq=j;
                 }
             }
         }
     }
     
        pair<int,int> loc = {tp, tq};
        find_closed_loop(loc,op_mat);

        pair<int,char> p;
        int maxi1=0,ind1=-1,maxi2=0,ind2=-1;
     for(int i=0; i<rows; i++)
     {
         int count=0;
         for(int j=0; j<cols; j++)
         {
             if(op_mat[i][j]>=0) count++;
         }
         if(maxi1<count)
         {
             maxi1 = count;
             ind1 = i;
         }
     }
     for(int j=0; j<cols; j++)
     {
         int count=0;
         for(int i=0; i<rows; i++)
         {
             if(op_mat[i][j]>=0) count++;
         }
         if(maxi2<count)
         {
             maxi2 = count;
             ind2 = j;
         }
     }
     if(maxi1>maxi2) 
  {
   p = {ind1,'r'};
  }
  else
  {
   p = {ind2,'c'};
  }
     
     if(p.second=='r')
     {
      u[p.first] = 0;
     }
     else 
     {
      v[p.first] = 0;
     }

     int rem = true;
     while(rem)
     {
         int count = 0;
         for(int i=0; i<rows; i++)
         {
             for(int j=0; j<cols; j++)
             {
                 if(op_mat[i][j]>=0)
                 {
                     if(u[i]!=INT_MAX && v[j]==INT_MAX)
                     {
                         v[j] = cost_mat[i][j] - u[i];
                     }
                     else if(u[i]==INT_MAX && v[j]!=INT_MAX)
                     {
                         u[i] = cost_mat[i][j] - v[j];
                     }
                 }
             }
         }

         for(int i=0; i<rows; i++)
         {
             if(u[i] != INT_MAX) count++;
         }
         for(int j=0; j<cols; j++)
         {
             if(v[j] != INT_MAX) count++;
         }

         if(count == rows + cols) rem = false;
     }
     
        //cal_u_v(u,v,output,cost);
        status = false;
     for(int i=0; i<rows; i++)
     {
         for(int j=0; j<cols; j++)
         {
             if(op_mat[i][j] < 0)
             {
                 delta[i][j] = cost_mat[i][j] - (u[i] + v[j]);
                 if(delta[i][j] < 0)
                 {
                  status = true;
                 }
             }
         }
     }

        //status = cal_opportunity_cost(delta,output,cost,u,v);

        fill(u.begin(),u.end(),INT_MAX);
        fill(v.begin(),v.end(),INT_MAX);
    }
}

void solve(vector<vector<int> > &cost_mat, vector<int> &supply, vector<int> &demand)
{
 int rows = cost_mat.size(), cols=cost_mat[0].size();

 int tot_supply = 0, tot_demand = 0;
 for(int i=0;i<rows;i++) 
 {
  tot_supply += supply[i];
 }

    for(int i=0;i<cols;i++)
    {
     tot_demand += demand[i];
    }

 // ADD DUMMY ROW/COLUMN IF DEMAND-SUPPLY IS NOT EQUAL
 demandSupplyEqualization(cost_mat, supply, demand, tot_supply, tot_demand, rows, cols);
 
 vector<vector<int> > op_mat(rows,vector<int>(cols, -1));
 vector<bool> isDone_row(rows,false);
    vector<bool> isDone_col(cols,false);
    vector<int> row_min_diff(rows,-1);
    vector<int> column_min_diff(cols,-1);

    int last = 1, tot_basic_vars = rows + cols-1;
    if(tot_demand != tot_supply)
    {
     tot_basic_vars--;
    } 

    VAM_Method(cost_mat, supply, demand, op_mat, isDone_row, isDone_col, row_min_diff, column_min_diff, last, tot_basic_vars);

    cout<<"INITIAL BASIC FEASIBLE SOLUTION: \n";
    int sum_cost = 0,cnt=0,pi=0, qi=0;
    int mini=INT_MAX;
    for(int i=0;i<rows;i++)
    {
        for(int j=0;j<cols;j++)
        {
            if(op_mat[i][j]>=0)
                cout<<"X"<<i<<j<<"= "<<op_mat[i][j]<<"\n";
            if(op_mat[i][j]>=0)
            {
                cnt++;
                sum_cost += op_mat[i][j]*cost_mat[i][j]; 
            }
            else
            {
                if(mini>cost_mat[i][j] && cost_mat[i][j] > 0) 
                {
                    mini = cost_mat[i][j];
                    pi=i;
                    qi=j;
                }
            }
        }
    }
    cout<<"\n";

    cout<<"BY VAM METHOD OPTIMUM COST  = "<<sum_cost<<endl;

    if(cnt == rows + cols -1)
        cout<<"SOLUTION IS NON DEGENERATE\n";
    else
    {
        cout<<"SOLUTION IS DEGENERATE AND DUMMY VALUE IS ASSIGNED TO MINIMUM OF UNOCCUPIED CELLS.\n";
        op_mat[pi][qi] = 0;
    }

    MODI_Method(cost_mat, op_mat);

    cout<<"OPTIMUM SOLUTION USING MODI METHOD: \n";
    sum_cost = 0;
    for(int i=0;i<rows;i++)
    {
        for(int j=0;j<cols;j++)
        {
            if(op_mat[i][j]>=0)
                cout<<"X"<<i<<j<<"= "<<op_mat[i][j]<<"\n";
            if(op_mat[i][j]<0) op_mat[i][j] = 0;
                sum_cost += op_mat[i][j]*cost_mat[i][j];
        }
    }
    cout<<"BY MODI METHOD OPTIMUM COST = "<<sum_cost<<endl;
}

int main()
{
 FAST;
 
 int n,m;
 cin>>n>>m;
 
 vector<vector<int> > cost_mat(n,vector<int>(m));
 for(int i=0; i<n; i++)
 {
     for(int j=0; j<m; j++)
     {
         cin>>cost_mat[i][j];
     }
 }
 
 vector<int> supply(n), demand(m);
 for(int i=0; i<n; i++)
 {
     cin>>supply[i];
 }
 
 for(int i=0; i<m; i++)
 {
     cin>>demand[i];
 }

 /*vector<vector<int>> cost_mat = {{19,30,50,10},{70,30,40,60},{40,8,70,20}};
 vector<int> supply = {7,9,18};
 vector<int> demand = {5,8,7,14};
 
 
 vector<vector<int>> cost_mat={{5,8,6,6,3},{4,7,7,6,5},{8,4,6,6,4}};
 vector<int> supply={8,5,9};
 vector<int> demand={4,4,5,4,8};

 vector<vector<int>> cost_mat={{19,30, 50, 10}, {70, 30, 40, 60}, {40, 8, 70, 20}};
 vector<int> supply = {7, 9, 18};
 vector<int> demand = {5, 8, 7, 14};*/

 solve(cost_mat, supply, demand);
}
