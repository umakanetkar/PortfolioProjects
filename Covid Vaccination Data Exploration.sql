#Followed tutorial by AlexTheAnalyst(YouTube)
select * 
from Project1..CovidDeaths
order by 3,4

--select * 
--from Project1..CovidVaccinations
--order by 3,4

--select data that we are using

select location, date, total_cases, new_cases, total_deaths, population
from Project1..CovidDeaths
order by 1,2

-- looking at total cases vs total deaths
-- this shows liklihood of dying if a US resident contracts covid
select location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
from Project1..CovidDeaths
where location like '%states%'
order by 1,2

-- looking at total cases vs population
-- shows what % of population got covid 
select location, date, population, total_cases, (total_cases/population)*100 as PercentPopulationInfected
from Project1..CovidDeaths
where location like '%states%'
order by 1,2

-- looking at countries with highest infection rate compared to population
select location, population, MAX(total_cases) as HighestInfectionCount, MAX(total_cases/population)*100 as
PercentPopulationInfected
from Project1..CovidDeaths
--where location like '%states%'
Group by location, population
order by PercentPopulationInfected desc

-- showing countries with highest death count per population
select location, MAX(cast(total_deaths as int)) as TotalDeathCount
from Project1..CovidDeaths
--where location like '%states%'
where continent is not null
Group by location
order by TotalDeathCount desc

-- breaking things down by continent
select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
from Project1..CovidDeaths
--where location like '%states%'
where continent is not null
Group by continent
order by TotalDeathCount desc

-- showing continents with highest deathcount
select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
from Project1..CovidDeaths
--where location like '%states%'
where continent is not null
Group by continent
order by TotalDeathCount desc

-- global numbers
select  SUM((new_cases)) as total_cases, SUM(cast(new_deaths as int)) as total_deaths,SUM(cast(New_deaths as int))/SUM(New_Cases) * 100 as DeathPercentage
--, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
from Project1..CovidDeaths
--where location like '%states%'
where continent is not null
--group by date
order by 1,2

--looking at total population vs vaccinations

Select dea.continent, dea.location, dea.date, dea.population , vac.new_vaccinations
, SUM(Convert(int,vac.new_vaccinations)) OVER (Partition by dea.location order by dea.location, dea.date)
as RollingPeopleVaccinated--, (RollingPeopleVaccinated/population)*100
from Project1..CovidDeaths dea
join Project1..CovidVaccinations vac
  on dea.location=vac.location
  and dea.date=vac.date
where dea.continent is not null
order by 2,3


--Using CTE
With PopvsVac (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
as 
( 
Select dea.continent,dea.location, dea.date, dea.population , vac.new_vaccinations
, SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location order by dea.location, dea.date)
as RollingPeopleVaccinated
From Project1..CovidDeaths dea
join Project1..CovidVaccinations vac 
  on dea.location=vac.location
  and  dea.date=vac.date
where dea.continent is not null
 )
 Select *, (RollingPeopleVaccinated/Population)*100
 From PopvsVac



 -- Using TEMP table
 DROP Table if exists #PercentPopulationVaccinated
 Create Table #PercentPopulationVaccinated
 (
 Continent nvarchar(255),
 Location nvarchar(255),
 Date datetime,
 Population numeric,
 New_vaccinations numeric,
 RollingPeopleVaccinated numeric
 )
 Insert into #PercentPopulationVaccinated
 Select dea.continent,dea.location, dea.date, dea.population , vac.new_vaccinations
, SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location order by dea.location, dea.date)
as RollingPeopleVaccinated
From Project1..CovidDeaths dea
join Project1..CovidVaccinations vac 
  on dea.location=vac.location
  and  dea.date=vac.date
where dea.continent is not null

 Select *, (RollingPeopleVaccinated/Population)*100
 From #PercentPopulationVaccinated
 Select dea.continent,dea.location, dea.date, dea.population , vac.new_vaccinations
, SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location order by dea.location, dea.date)
as RollingPeopleVaccinated
From Project1..CovidDeaths dea
join Project1..CovidVaccinations vac 
  on dea.location=vac.location
  and  dea.date=vac.date
where dea.continent is not null



 --Creating View to store data for later visualisations
 Create View PercentPopulationVaccinated1 as 
 Select dea.continent,dea.location, dea.date, dea.population , vac.new_vaccinations
, SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location order by dea.location, dea.date)
as RollingPeopleVaccinated
From Project1..CovidDeaths dea
join Project1..CovidVaccinations vac 
  on dea.location=vac.location
  and  dea.date=vac.date
where dea.continent is not null
