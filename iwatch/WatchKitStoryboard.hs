module IWatch.WatchKitStoryboard where
import System.Random

{-|

EJEMPLO DE ARCHIVO .storyboard A PARTIR DE DONDE SE BASAN LAS REPRESENTACIONES HASKELL (ESTRUCTURALMENTE SIMILARES)

<document colorMatched="YES" initialViewController="7n7-b6-a4v" propertyAccessControl="none" systemVersion="16B2555" targetRuntime="watchKit" toolsVersion="11542" type="com.apple.InterfaceBuilder.WatchKit.Storyboard" useAutolayout="YES" useTraitCollections="YES" version="3.0">
		<dependencies>
				<deployment identifier="watchOS"/>
				<plugIn identifier="com.apple.InterfaceBuilder.IBCocoaTouchPlugin" version="11524"/>
				<plugIn identifier="com.apple.InterfaceBuilder.IBWatchKitPlugin" version="11508"/>
		</dependencies>
		<scenes>
				<!--GX Initial Controller-->
				<scene sceneID="XW7-5R-O67">
						<objects>
								<controller customClass="WKInterfaceController" customModule="iOSMainPanel_WatchKit_App" customModuleProvider="target" id="7n7-b6-a4v"/>
						</objects>
						<point key="canvasLocation" x="0" y="0"/>
				</scene>
				<!--iosmainpanelwatchos-->
				<scene sceneID="xEn-PZ-pT7">
						<objects>
								<controller customClass="GXInterfaceController_iOSMainPanelWatchOS" customModule="iOSMainPanel_WatchKit_Extension" id="7uG-3I-2pS">
										<items>
												<group id="Sfs-6x-IH9" layout="vertical" width="1">
														<items>
																<group id="9H3-Eb-1B8" layout="vertical" width="1">
																		<items>
																				<label id="8N3-49-U2H" text="Simple Text" width="1"/>
																				<label id="H57-Fb-aQw" text=" " width="1"/>
																		</items>
																</group>
																<label id="75z-hj-y88" text="Text Block" width="1"/>
																<button id="fNO-09-yJZ" title="HelloWorld" width="1">
																		<connections>
																				<action destination="7uG-3I-2pS" id="Z00-Oi-52l" selector="gxControlActionSelector_Helloworld"/>
																		</connections>
																</button>
														</items>
												</group>
										</items>
										<connections>
												<outlet destination="Sfs-6x-IH9" id="mgx-60-NA2" property="gxControl_maintable"/>
												<outlet destination="9H3-Eb-1B8" id="4Zk-2y-fQ7" property="gxControl_simpletext"/>
												<outlet destination="8N3-49-U2H" id="wM4-1B-64O" property="gxControlLabel_simpletext"/>
												<outlet destination="H57-Fb-aQw" id="Oge-n4-FY4" property="gxControlEditor_simpletext"/>
												<outlet destination="75z-hj-y88" id="82C-H9-iff" property="gxControl_textblock1"/>
												<outlet destination="fNO-09-yJZ" id="lE9-S8-wMH" property="gxControl_helloworld"/>
										</connections>
								</controller>
						</objects>
						<point key="canvasLocation" x="300" y="0"/>
				</scene>
				<!--sdtgrid-->
				<scene sceneID="2Tz-CZ-t2l">
						<objects>
								<controller customClass="GXInterfaceController_SDTGrid" customModule="iOSMainPanel_WatchKit_Extension" id="lOt-2e-qO3">
										<items>
												<group id="3gM-12-SV1" layout="vertical" width="1">
														<items>
																<table id="1CL-kg-bkA" width="1">
																		<items>
																				<tableRow customClass="GXRowController_Grid1_Layout1" customModule="iOSMainPanel_WatchKit_Extension" id="AK1-A4-s92" identifier="Layout1" width="1">
																						<group id="28k-JU-1zB" layout="vertical" width="1">
																								<items>
																										<label id="B7Z-oH-1g3" text=" " width="1"/>
																								</items>
																						</group>
																						<connections>
																								<outlet destination="28k-JU-1zB" id="q95-7o-32S" property="gxControl_grid1table"/>
																								<outlet destination="B7Z-oH-1g3" id="37a-9i-tgq" property="gxControl_ctldescription"/>
																						</connections>
																				</tableRow>
																		</items>
																</table>
														</items>
												</group>
										</items>
										<connections>
												<outlet destination="3gM-12-SV1" id="4H4-J3-1sX" property="gxControl_maintable"/>
												<outlet destination="1CL-kg-bkA" id="San-n1-f14" property="gxControl_grid1"/>
										</connections>
								</controller>
						</objects>
						<point key="canvasLocation" x="600" y="0"/>
				</scene>
		</scenes>
</document>

-}

-- ObjectID representa identificates usando es muchos lados, correspondientes a 8 catacteres alfanumericos (ej. "3gM-12-SV1")
data ObjectID = ObjectID Char Char Char Char Char Char Char Char
  deriving (Show, Eq)

instance Random ObjectID where
    random g = let { (c1, g1) = randomObjIdChar g;
                     (c2, g2) = randomObjIdChar g1;
                     (c3, g3) = randomObjIdChar g2;
                     (c4, g4) = randomObjIdChar g3;
                     (c5, g5) = randomObjIdChar g4;
                     (c6, g6) = randomObjIdChar g5;
                     (c7, g7) = randomObjIdChar g6;
                     (c8, _)  = randomObjIdChar g7 } in (ObjectID c1 c2 c3 c4 c5 c6 c7 c8, g7)
    randomR (_,_) = random

randomObjIdChar :: RandomGen g => g -> (Char, g)
randomObjIdChar g = let (n, gn) = randomR ((1, 3) :: (Int, Int)) g in
     case n of
       1 -> randomR ('0', '9') gn
       2 -> randomR ('A', 'Z') gn
       3 -> randomR ('a', 'z') gn
       _ -> error "How?"

type Point = (Int, Int)

data HorizontalAlignment = HorizontalAlignmentDefault | HorizontalAlignmentLeft | HorizontalAlignmentCenter | HorizontalAlignmentRight
  deriving (Show, Eq)

data VerticalAlignment = VerticalAlignmentDefault | VerticalAlignmentTop | VerticalAlignmentCenter | VerticalAlignmentBottom
  deriving (Show, Eq)

data Dependency = PlugIn {
      plugInIdentifier     :: String,
      version              :: Int
    }           | Deployment {
      deploymentIdentifier :: String
    } deriving (Show, Eq)

-- Document representa el nodo raiz
data Document = Document {
      dependencies          :: [Dependency],
      scenes                :: [Scene],
      initialViewController :: ObjectID
    } deriving (Show, Eq)

-- Scene representa los nodos correspondientes a cada pantalla
data Scene = Scene {
      sceneId             :: ObjectID,
      sceneDescription    :: String,
      sceneObjects        :: [SceneObject],
      sceneCanvasLocation :: Point
} deriving (Show, Eq)

data SceneObject = SceneObject ControllerBase ControllerSpecific
  deriving (Show, Eq)

data ControllerSpecific = Controller {
      customClass          :: String,
      customModule         :: String,
      customModuleProvider :: String
    }
                         | NotificationController {
      notificationCategory :: NotificationCategory
    } deriving (Show, Eq)

data ControllerBase = ControllerBase {
      controllerId          :: ObjectID,
      controllerIdentifier  :: String,
      controllerItems       :: [Item],
      controllerConnections :: [Connection]
    } deriving (Show, Eq)

data NotificationCategory = NotificationCategory {
      notificationCategoryId         :: ObjectID,
      notificationCategoryIdentifier :: String
    } deriving (Show, Eq)

-- Item representa a los elementos (con diferentes especificaciones) que aparecen en una pantalla
data Item = Item ItemBase ItemSpecific
  deriving (Show, Eq)

data ItemSpecific = GroupItem {
      groupItemVerticalLayout :: Bool
    }
                  | LabelItem {
      labelItemText           :: String
    }
                  | ButtonItem {
      buttonItemTitle         :: String
    }
                  | TableItem {
    }
                  | TableRowItem {
      tableRowIdentifier           :: String,
      tableRowSelectable           :: Bool,
      tableRowCustomClass          :: String,
      tableRowCustomModule         :: String,
      tableRowCustomModuleProvider :: String,
      tableRowRootItem             :: Item
    } deriving (Show, Eq)

data ItemBase = ItemBase {
      itemId                  :: ObjectID,
      itemHorizontalAlignment :: HorizontalAlignment,
      itemVerticalAlignment   :: VerticalAlignment,
      itemSubitems            :: [Item],
      itemConnections         :: [Connection]
    } deriving (Show, Eq)

-- Connection representa los nodos hijos de connections, que corresponden a conexiones entre elementos del storyboard y el fuente swift asociado
data Connection = Connection ConnectionBase ConnectionSpecific
  deriving (Show, Eq)

data ConnectionBase = ConnectionBase {
      connectionId          :: ObjectID,
      connectionDestination :: ObjectID
    } deriving (Show, Eq)

data ConnectionSpecific = Outlet {
      outletPropertyName    :: String
    }
                        | Segue {
      segueKind             :: String,
      segueRelationship     :: String
    }
                        | Action {
      actionSelector        :: String
    } deriving (Show, Eq)
