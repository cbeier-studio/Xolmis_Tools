unit test_birdnames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestFormatBirdDomestic }

  TTestFormatBirdDomestic = class(TTestCase)
  published
    procedure TestDomestic;
  end;

  { TTestFormatBirdForm }

  TTestFormatBirdForm = class(TTestCase)
  published
    procedure TestFormSpecies;
    procedure TestFormUndescribed;
    procedure TestFormGenus;
    procedure TestFormSubspecies;
    procedure TestFormSlash1;
    procedure TestFormSlash2;
    procedure TestFormSlash3;
  end;

  { TTestFormatBirdHybrid }

  TTestFormatBirdHybrid = class(TTestCase)
  published
    procedure TestHybridIntrageneric;
    procedure TestHybridDomestic;
    procedure TestHybridDomestic1;
    procedure TestHybridDomestic2;
    procedure TestHybridIntergeneric;
    procedure TestHybridIntergenericSpuh;
    procedure TestHybridBetweenSpuhs;
    procedure TestHybridBetweenGroups;
    procedure TestHybridBetweenGroups1;
    procedure TestHybridBetweenGroups2;
    procedure TestHybridBetweenGroups3;
    procedure TestHybridParenthesis;
  end;

  { TTestFormatBirdIntergrade }

  TTestFormatBirdIntergrade = class(TTestCase)
  published
    procedure TestIntergrade;
    procedure TestIntergradeWithGroup1;
    procedure TestIntergradeWithGroup2;
    procedure TestIntergradeBetweenGroups;
    procedure TestIntergradeBetweenGroups1;
    procedure TestIntergradeParenthesis;
    procedure TestIntergradeSlash;
  end;

  { TTestFormatBirdMonotypicGroup }

  TTestFormatBirdMonotypicGroup = class(TTestCase)
  published
    procedure TestMonotypicGroup;
  end;

  { TTestFormatBirdPolitypicGroup }

  TTestFormatBirdPolitypicGroup = class(TTestCase)
  published
    procedure TestPolitypicGroupBrackets;
    procedure TestPolitypicGroupSlash;
  end;

  { TTestFormatBirdSlash }

  TTestFormatBirdSlash = class(TTestCase)
  published
    procedure TestSlashIntrageneric;
    procedure TestSlashIntrageneric3;
    procedure TestSlashIntergeneric;
  end;

  { TTestFormatBirdSpuh }

  TTestFormatBirdSpuh = class(TTestCase)
  published
    procedure TestSpuh;
    procedure TestSpuh1;
    procedure TestSpuhSlash;
    procedure TestSpuhSlash1;
    procedure TestSpuhSlash3;
    procedure TestSpuhSlash4;
    procedure TestSpuhSlash5;
    procedure TestSpuhFamily;
    procedure TestSpuhFamily1;
    procedure TestSpuhOrder;
    procedure TestSpuhEagle;
  end;

implementation

uses
  utils_taxonomy;

{ TTestFormatBirdDomestic }

procedure TTestFormatBirdDomestic.TestDomestic;
var
  aName, aResult: String;
begin
  aName := 'Anser anser (Domestic type)';
  aResult := '<i>Anser anser</i> <font color="cornflowerblue">(Domestic type)</font>';
  AssertEquals('Domestic bird name was formatted incorrectly.', aResult, FormatDomestic(aName));
end;

{ TTestFormatBirdForm }

procedure TTestFormatBirdForm.TestFormGenus;
var
  aName, aResult: String;
begin
  aName := 'Caprimulgus [undescribed Timor form]';
  aResult := '<i>Caprimulgus</i> <font color="cadetblue">[undescribed Timor form]</font>';
  AssertEquals('Form bird name generic was formatted incorrectly.', aResult, FormatForm(aName));
end;

procedure TTestFormatBirdForm.TestFormSlash1;
var
  aName, aResult: String;
begin
  aName := 'Larus glaucoides thayeri/kumlieni';
  aResult := '<i>Larus glaucoides <font color="cadetblue">thayeri/kumlieni</font></i>';
  AssertEquals('Form bird name slash was formatted incorrectly.', aResult, FormatForm(aName));
end;

procedure TTestFormatBirdForm.TestFormSlash2;
var
  aName, aResult: String;
begin
  aName := 'Numenius phaeopus phaeopus/alboaxillaris/variegatus';
  aResult := '<i>Numenius phaeopus <font color="cadetblue">phaeopus/alboaxillaris/variegatus</font></i>';
  AssertEquals('Form bird name slash was formatted incorrectly.', aResult, FormatForm(aName));
end;

procedure TTestFormatBirdForm.TestFormSlash3;
var
  aName, aResult: String;
begin
  aName := 'Oceanodroma leucorhoa/socorroensis (dark-rumped)';
  aResult := '<i>Oceanodroma leucorhoa/socorroensis</i> <font color="cadetblue">(dark-rumped)</font>';
  AssertEquals('Form bird name slash was formatted incorrectly.', aResult, FormatForm(aName));
end;

procedure TTestFormatBirdForm.TestFormSpecies;
var
  aName, aResult: String;
begin
  aName := 'Chloephaga picta (White-breasted)';
  aResult := '<i>Chloephaga picta</i> <font color="cadetblue">(White-breasted)</font>';
  AssertEquals('Form bird name specific was formatted incorrectly.', aResult, FormatForm(aName));
end;

procedure TTestFormatBirdForm.TestFormSubspecies;
var
  aName, aResult: String;
begin
  aName := 'Larus fuscus taimyrensis';
  aResult := '<i>Larus fuscus <font color="cadetblue">taimyrensis</font></i>';
  AssertEquals('Form bird name subspecific was formatted incorrectly.', aResult, FormatForm(aName));
end;

procedure TTestFormatBirdForm.TestFormUndescribed;
var
  aName, aResult: String;
begin
  aName := 'Tachyeres pteneres [undescribed form]';
  aResult := '<i>Tachyeres pteneres</i> <font color="cadetblue">[undescribed form]</font>';
  AssertEquals('Form bird name generic was formatted incorrectly.', aResult, FormatForm(aName));
end;

{ TTestFormatBirdHybrid }

procedure TTestFormatBirdHybrid.TestHybridBetweenGroups;
var
  aName, aResult: String;
begin
  aName := 'Aythya collaris x marila/affinis';
  aResult := '<i>Aythya collaris</i> <font color="darkslateblue"><b>×</b></font> <i>marila/affinis</i>';
  AssertEquals('Hybrid bird name between groups was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridBetweenGroups1;
var
  aName, aResult: String;
begin
  aName := 'Anser albifrons x Branta hutchinsii/canadensis';
  aResult := '<i>Anser albifrons</i> <font color="darkslateblue"><b>×</b></font> <i>Branta hutchinsii/canadensis</i>';
  AssertEquals('Hybrid bird name between groups was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridBetweenGroups2;
var
  aName, aResult: String;
begin
  aName := 'Bucephala clangula/islandica x Lophodytes cucullatus';
  aResult := '<i>Bucephala clangula/islandica</i> <font color="darkslateblue"><b>×</b></font> <i>Lophodytes cucullatus</i>';
  AssertEquals('Hybrid bird name between groups was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridBetweenGroups3;
var
  aName, aResult: String;
begin
  aName := 'Anser caerulescens/rossii x Branta hutchinsii/canadensis';
  aResult := '<i>Anser caerulescens/rossii</i> <font color="darkslateblue"><b>×</b></font> <i>Branta hutchinsii/canadensis</i>';
  AssertEquals('Hybrid bird name between groups was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridBetweenSpuhs;
var
  aName, aResult: String;
begin
  aName := 'Glossopsitta/Trichoglossus sp. (hybrid)';
  aResult := '<i>Glossopsitta/Trichoglossus</i> <b>sp.</b> <font color="darkslateblue">(hybrid)</font>';
  AssertEquals('Hybrid bird name intergeneric between spuhs was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridDomestic;
var
  aName, aResult: String;
begin
  aName := 'Anser anser x cygnoides (Domestic type)';
  aResult := '<i>Anser anser</i> <font color="darkslateblue"><b>×</b></font> <i>cygnoides</i> <font color="cornflowerblue">(Domestic type)</font>';
  AssertEquals('Hybrid bird name domestic was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridDomestic1;
var
  aName, aResult: String;
begin
  aName := 'Anser anser (Domestic type) x Branta canadensis';
  aResult := '<i>Anser anser</i> <font color="cornflowerblue">(Domestic type)</font> <font color="darkslateblue"><b>×</b></font> <i>Branta canadensis</i>';
  AssertEquals('Hybrid bird name domestic was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridDomestic2;
var
  aName, aResult: String;
begin
  aName := 'Anser sp. (Domestic type) x Branta canadensis';
  aResult := '<i>Anser</i> <b>sp.</b> <font color="cornflowerblue">(Domestic type)</font> <font color="darkslateblue"><b>×</b></font> <i>Branta canadensis</i>';
  AssertEquals('Hybrid bird name domestic was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridIntergeneric;
var
  aName, aResult: String;
begin
  aName := 'Antilophia galeata x Chiroxiphia pareola';
  aResult := '<i>Antilophia galeata</i> <font color="darkslateblue"><b>×</b></font> <i>Chiroxiphia pareola</i>';
  AssertEquals('Hybrid bird name intergeneric was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridIntergenericSpuh;
var
  aName, aResult: String;
begin
  aName := 'Aglaiocercus kingii x Trochilidae sp. (Bogota Sunangel)';
  aResult := '<i>Aglaiocercus kingii</i> <font color="darkslateblue"><b>×</b></font> Trochilidae <b>sp.</b> <font color="teal">(Bogota Sunangel)</font>';
  AssertEquals('Hybrid bird name intergeneric with spuh was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridIntrageneric;
var
  aName, aResult: String;
begin
  aName := 'Vireo atricapilla x griseus';
  aResult := '<i>Vireo atricapilla</i> <font color="darkslateblue"><b>×</b></font> <i>griseus</i>';
  AssertEquals('Hybrid bird name intrageneric was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

procedure TTestFormatBirdHybrid.TestHybridParenthesis;
var
  aName, aResult: String;
begin
  aName := 'Vermivora chrysoptera x cyanoptera (F1 hybrid)';
  aResult := '<i>Vermivora chrysoptera</i> <font color="darkslateblue"><b>×</b></font> <i>cyanoptera</i> <font color="darkslateblue">(F1 hybrid)</font>';
  AssertEquals('Hybrid bird name was formatted incorrectly.', aResult, FormatHybrid(aName));
end;

{ TTestFormatBirdIntergrade }

procedure TTestFormatBirdIntergrade.TestIntergrade;
var
  aName, aResult: String;
begin
  aName := 'Himantopus mexicanus mexicanus x melanurus';
  aResult := '<i>Himantopus mexicanus mexicanus</i> <font color="goldenrod"><b>×</b></font> <i>melanurus</i>';
  AssertEquals('Intergrade bird name was formatted incorrectly.', aResult, FormatIntergrade(aName));
end;

procedure TTestFormatBirdIntergrade.TestIntergradeBetweenGroups;
var
  aName, aResult: String;
begin
  aName := 'Setophaga petechia [erithachorides Group x petechia Group]';
  aResult := '<i>Setophaga petechia</i> <font color="green">[<i>erithachorides</i> Group <font color="goldenrod"><b>×</b></font> <i>petechia</i> Group]</font>';
  AssertEquals('Intergrade bird name between groups was formatted incorrectly.', aResult, FormatIntergrade(aName));
end;

procedure TTestFormatBirdIntergrade.TestIntergradeBetweenGroups1;
var
  aName, aResult: String;
begin
  aName := 'Canachites canadensis [canadensis Group] x franklinii/isleibi';
  aResult := '<i>Canachites canadensis</i> <font color="green">[<i>canadensis</i> Group]</font> <font color="goldenrod"><b>×</b></font> <font color="green"><i>franklinii/isleibi</i></font>';
  AssertEquals('Intergrade bird name between groups was formatted incorrectly.', aResult, FormatIntergrade(aName));
end;

procedure TTestFormatBirdIntergrade.TestIntergradeParenthesis;
var
  aName, aResult: String;
begin
  aName := 'Motacilla flava (superciliaris intergrade)';
  aResult := '<i>Motacilla flava</i> <font color="goldenrod">(<i>superciliaris</i> intergrade)</font>';
  AssertEquals('Intergrade bird name was formatted incorrectly.', aResult, FormatIntergrade(aName));
end;

procedure TTestFormatBirdIntergrade.TestIntergradeSlash;
var
  aName, aResult: String;
begin
  aName := 'Sterna hirundo hirundo/tibetana x longipennis';
  aResult := '<i>Sterna hirundo <font color="green">hirundo/tibetana</font></i> <font color="goldenrod"><b>×</b></font> <i>longipennis</i>';
  AssertEquals('Intergrade bird name with slash was formatted incorrectly.', aResult, FormatIntergrade(aName));
end;

procedure TTestFormatBirdIntergrade.TestIntergradeWithGroup1;
var
  aName, aResult: String;
begin
  aName := 'Gymnorhina tibicen [tibicen Group] x dorsalis';
  aResult := '<i>Gymnorhina tibicen</i> <font color="green">[<i>tibicen</i> Group]</font> <font color="goldenrod"><b>×</b></font> <i>dorsalis</i>';
  AssertEquals('Intergrade bird name with group before x was formatted incorrectly.', aResult, FormatIntergrade(aName));
end;

procedure TTestFormatBirdIntergrade.TestIntergradeWithGroup2;
var
  aName, aResult: String;
begin
  aName := 'Leucosticte tephrocotis littoralis x [tephrocotis Group]';
  aResult := '<i>Leucosticte tephrocotis littoralis</i> <font color="goldenrod"><b>×</b></font> <font color="green">[<i>tephrocotis</i> Group]</font>';
  AssertEquals('Intergrade bird name with group after x was formatted incorrectly.', aResult, FormatIntergrade(aName));
end;

{ TTestFormatBirdMonotypicGroup }

procedure TTestFormatBirdMonotypicGroup.TestMonotypicGroup;
var
  aName, aResult: String;
begin
  aName := 'Dendrocygna autumnalis fulgens';
  aResult := '<i>Dendrocygna autumnalis <font color="green">fulgens</font></i>';
  AssertEquals('Monotypic group bird name was formatted incorrectly.', aResult, FormatMonotypicGroup(aName));
end;

{ TTestFormatBirdPolitypicGroup }

procedure TTestFormatBirdPolitypicGroup.TestPolitypicGroupBrackets;
var
  aName, aResult: String;
begin
  aName := 'Branta canadensis [canadensis Group]';
  aResult := '<i>Branta canadensis</i> <font color="green">[<i>canadensis</i> Group]</font>';
  AssertEquals('Politypic group bird name was formatted incorrectly.', aResult, FormatPolitypicGroup(aName));
end;

procedure TTestFormatBirdPolitypicGroup.TestPolitypicGroupSlash;
var
  aName, aResult: String;
begin
  aName := 'Branta canadensis occidentalis/fulva';
  aResult := '<i>Branta canadensis <font color="green">occidentalis/fulva</font></i>';
  AssertEquals('Politypic group bird name was formatted incorrectly.', aResult, FormatPolitypicGroup(aName));
end;

{ TTestFormatBirdSlash }

procedure TTestFormatBirdSlash.TestSlashIntergeneric;
var
  aName, aResult: String;
begin
  aName := 'Nomonyx dominicus/Oxyura jamaicensis';
  aResult := '<font color="maroon"><i>Nomonyx dominicus/Oxyura jamaicensis</i></font>';
  AssertEquals('Slash bird name intergeneric was formatted incorrectly.', aResult, FormatSlash(aName));
end;

procedure TTestFormatBirdSlash.TestSlashIntrageneric;
var
  aName, aResult: String;
begin
  aName := 'Dendrocygna bicolor/javanica';
  aResult := '<i>Dendrocygna <font color="maroon">bicolor/javanica</font></i>';
  AssertEquals('Slash bird name intrageneric was formatted incorrectly.', aResult, FormatSlash(aName));
end;

procedure TTestFormatBirdSlash.TestSlashIntrageneric3;
var
  aName, aResult: String;
begin
  aName := 'Melanitta fusca/deglandi/stejnegeri';
  aResult := '<i>Melanitta <font color="maroon">fusca/deglandi/stejnegeri</font></i>';
  AssertEquals('Slash bird name intrageneric was formatted incorrectly.', aResult, FormatSlash(aName));
end;

{ TTestFormatBirdSpuh }

procedure TTestFormatBirdSpuh.TestSpuh;
var
  aName, aResult: String;
begin
  aName := 'Tinamus sp.';
  aResult := '<font color="purple"><i>Tinamus</i></font> <b>sp.</b>';
  AssertEquals('Spuh bird name was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuh1;
var
  aName, aResult: String;
begin
  aName := 'Chrysococcyx sp. (bronze-cuckoo sp.)';
  aResult := '<font color="purple"><i>Chrysococcyx</i></font> <b>sp.</b> <font color="teal">(bronze-cuckoo sp.)</font>';
  AssertEquals('Spuh bird name was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhEagle;
var
  aName, aResult: String;
begin
  aName := 'Buteo/eagle sp.';
  aResult := '<font color="purple"><i>Buteo</i>/eagle</font> <b>sp.</b>';
  AssertEquals('Spuh bird name with slash was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhFamily;
var
  aName, aResult: String;
begin
  aName := 'Phoenicopteridae sp.';
  aResult := '<font color="purple">Phoenicopteridae</font> <b>sp.</b>';
  AssertEquals('Family spuh bird name was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhFamily1;
var
  aName, aResult: String;
begin
  aName := 'Anatidae sp. (teal sp.)';
  aResult := '<font color="purple">Anatidae</font> <b>sp.</b> <font color="teal">(teal sp.)</font>';
  AssertEquals('Family spuh bird name was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhOrder;
var
  aName, aResult: String;
begin
  aName := 'Charadriiformes sp.';
  aResult := '<font color="purple">Charadriiformes</font> <b>sp.</b>';
  AssertEquals('Order spuh bird name was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhSlash;
var
  aName, aResult: String;
begin
  aName := 'Synoicus/Coturnix sp.';
  aResult := '<font color="purple"><i>Synoicus/Coturnix</i></font> <b>sp.</b>';
  AssertEquals('Spuh bird name with slash was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhSlash1;
var
  aName, aResult: String;
begin
  aName := 'Oceanitidae/Hydrobatidae sp. (dark-rumped)';
  aResult := '<font color="purple">Oceanitidae/Hydrobatidae</font> <b>sp.</b> <font color="teal">(dark-rumped)</font>';
  AssertEquals('Spuh bird name with slash was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhSlash3;
var
  aName, aResult: String;
begin
  aName := 'Mergellus/Lophodytes/Mergus sp.';
  aResult := '<font color="purple"><i>Mergellus/Lophodytes/Mergus</i></font> <b>sp.</b>';
  AssertEquals('Spuh bird name with slash was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhSlash4;
var
  aName, aResult: String;
begin
  aName := 'Pternistis/Francolinus/Peliperdix/Scleroptila sp.';
  aResult := '<font color="purple"><i>Pternistis/Francolinus/Peliperdix/Scleroptila</i></font> <b>sp.</b>';
  AssertEquals('Spuh bird name with slash was formatted incorrectly.', aResult, FormatSpuh(aName));
end;

procedure TTestFormatBirdSpuh.TestSpuhSlash5;
var
  aName, aResult: String;
begin
  aName := 'Apus pacificus/salimalii/leuconyx/cooki';
  aResult := '<font color="purple"><i>Apus pacificus/salimalii/leuconyx/cooki</i></font>';
  AssertEquals('Spuh bird name with slash was formatted incorrectly.', aResult, FormatSpuh(aName));
end;



initialization

  RegisterTest(TTestFormatBirdDomestic);
  RegisterTest(TTestFormatBirdForm);
  RegisterTest(TTestFormatBirdHybrid);
  RegisterTest(TTestFormatBirdIntergrade);
  RegisterTest(TTestFormatBirdMonotypicGroup);
  RegisterTest(TTestFormatBirdPolitypicGroup);
  RegisterTest(TTestFormatBirdSlash);
  RegisterTest(TTestFormatBirdSpuh);
end.

